;;; tray-builder.el --- Configure hydra builder -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/tray-builder
;; Keywords: lisp
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1") (transient "0.3.7.50-git"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helpers for generating transient prefixes

;;; Code:

(defcustom tray-builder-transient-doc-regexp-words '("forward"
                                                     "backward"
                                                     "up"
                                                     "down"
                                                     "undo"
                                                     "redo"
                                                     "next"
                                                     "more"
                                                     "previous"
                                                     "less"
                                                     "toggle"
                                                     "prev"
                                                     "previous"
                                                     "scroll")
  "List of words to indicate that generated commmand shouldn't exit transient."
  :type '(repeat string)
  :group 'tray-builder)

(defvar tray-builder-exclude-cmds
  '(ignore
    self-insert-command
    digit-argument
    undefined
    scroll-up-command
    scroll-down-command
    negative-argument)
  "List of commands to always exclude from `key-assist' output.")

(defvar tray-builder-assist-exclude-regexps
  "\\(^<\\|keymap\\|follow-link\\|compose-last-chars\\|drag-n-drop\\|menu\\|XF86Forward\\|XF86Back\\|help\\|iconify-frame\\|touch\\|mouse\\|wheel\\)\\|\\.\\."
  "List of regexps of commands to exclude from `key-assist' output.")

(defun tray-builder-help-fns-find-keymap-name (keymap)
  "Find the name of the variable with value KEYMAP.
Return nil if KEYMAP is not a valid keymap, or if there is no
variable with value KEYMAP."
  (when (keymapp keymap)
    (let ((name (catch 'found-keymap
                  (mapatoms (lambda (symb)
                              (when (and (boundp symb)
                                         (eq (symbol-value symb) keymap)
                                         (not (eq symb 'keymap)))
                                (throw 'found-keymap symb))))
                  nil)))
      ;; Follow aliasing.
      (or (ignore-errors (indirect-variable name)) name))))

(defun tray-builder-help-fns--most-relevant-active-keymap ()
  "Return the name of the most relevant active keymap.
The heuristic to determine which keymap is most likely to be
relevant to a user follows this order:

1. `keymap' text property at point
2. `local-map' text property at point
3. the `current-local-map'

This is used to set the default value for the interactive prompt
in `describe-keymap'.  See also `Searching the Active Keymaps'."
  (tray-builder-help-fns-find-keymap-name (or (get-char-property (point) 'keymap)
                         (if (get-text-property (point) 'local-map)
                             (get-char-property (point) 'local-map)
                           (current-local-map)))))

(defun tray-builder-copy-as-string (result)
  "Copy RESULT according to MODE-FN called with ARGS.
If RESULT is not a string, instead of MODE-FN emacs-lisp-mode will be used."
  (let ((content (tray-builder-prettify-vector result)))
    (kill-new content)
    (message content)
    content))

(defun tray-builder-shared-start (s1 s2)
  "Return the longest prefix S1 and S2 have in common."
  (declare (pure t)
           (side-effect-free t))
  (let ((search-length (min (length s1)
                            (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i)
                   (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun tray-builder-format-keymap-to-alist (keymap &optional symb-prefix)
  "Convert KEYMAP to alist and filter by SYMB-PREFIX."
  (when (keymapp keymap)
    (if-let ((name
              (when symb-prefix
                (car
                 (split-string (if (symbolp symb-prefix)
                                   (symbol-name symb-prefix)
                                 symb-prefix)
                               "-" t)))))
        (tray-builder-keymap-to-alist keymap
                                      (lambda (_k v)
                                        (and (symbolp v)
                                             (string-prefix-p name
                                                              (symbol-name
                                                               v)))))
      (tray-builder-keymap-to-alist keymap))))

(defun tray-builder-shortcut-pred (used-keys key)
  "Return non nil if KEY is a valid and not present in USED-KEYS."
  (and
   (key-valid-p key)
   (not (member key used-keys))))

(defun tray-builder-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (let ((parse-sexp-ignore-comments t))
    (when-let ((str-start (nth 8 (syntax-ppss (point)))))
      (goto-char str-start))
    (let ((init-pos (point))
          (pos)
          (count n))
      (while (and (not (= count 0))
                  (when-let ((end (ignore-errors
                                    (funcall fn)
                                    (point))))
                    (unless (= end (or pos init-pos))
                      (setq pos end))))
        (setq count (1- count)))
      (if (= count 0)
          pos
        (goto-char init-pos)
        nil))))

(defun tray-builder-elisp-forward-sexp (&optional arg)
  "Move forward across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (tray-builder-move-with 'forward-sexp arg))

(defun tray-builder-safe-substring (len word)
  "Substring WORD from zero to LEN."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun tray-builder-capitalize-variants (word)
  "Return list of words of WORD, but it with upcased letter."
  (let ((cands)
        (parts (split-string word "" t)))
    (dotimes (i (length parts))
      (let ((val (string-join (remove nil (list
                                           (when (> i 0)
                                             (string-join (seq-take parts i) ""))
                                           (upcase (nth i parts))
                                           (string-join (seq-drop parts (1+ i))
                                                        "")))
                              "")))
        (push val
              cands)))
    (reverse cands)))

(defun tray-builder-get-all-key-strategies (word len)
  "Generate preffered shortcut from WORD with length LEN."
  (let* ((parts (append (split-string word "[^a-z]" t)
                        (list (replace-regexp-in-string "[^a-z]" "" word))))
         (parts-len (length parts))
         (finalize (lambda (short)
                     (while (> len (length short))
                       (setq short (concat short (number-to-string (random 10)))))
                     (tray-builder-safe-substring len short)))
         (vars
          (mapcar finalize (tray-builder-capitalize-variants
                            (tray-builder-safe-substring len
                                                         (replace-regexp-in-string
                                                          "[^a-z]"
                                                          ""
                                                          word))))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (tray-builder-shared-start word it)))))
     #'>
     (seq-uniq (append
                vars
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'tray-builder-safe-substring n)
                                      parts)))
                 (number-sequence 1 (min len parts-len)))
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'tray-builder-safe-substring n)
                                      (reverse parts))))
                 (number-sequence 1 (min len parts-len))))))))

(defun tray-builder-minor-modes ()
  "List minor modes and their details.

Return a list of minor modes, each represented as a list containing the mode
function, variable, global status, and enabled state."
  (mapcan
   (lambda (fn)
     (let ((var (and (boundp fn)
                     fn))
           (ignore nil))
       (pcase fn
         ('auto-fill-function (setq ignore t))
         ('auto-fill-mode (setq var 'auto-fill-function))
         ('auto-save-mode (setq var 'buffer-auto-save-file-name))
         ('buffer-read-only (setq fn 'read-only-mode))
         ('edit-indirect--overlay (setq ignore t)))
       (and (not ignore)
            (fboundp fn)
            (commandp fn)
            (let (global enabled)
              (cond ((and (boundp 'global-minor-modes)
                          (memq fn global-minor-modes))
                     (setq global t)
                     (setq enabled t))
                    ((and (boundp 'local-minor-modes)
                          (memq fn local-minor-modes))
                     (setq enabled t))
                    ((or (get fn 'globalized-minor-mode)
                         (and var (not (local-variable-if-set-p var)))
                         (string-prefix-p "global-" (symbol-name fn)))
                     (setq global t)
                     (setq enabled (and var (symbol-value var))))
                    ((setq enabled (and var (symbol-value var)))))
              (list (list fn var global
                          (and enabled
                               t)))))))
   (sort minor-mode-list #'string<)))

(defun tray-builder-global-minor-modes ()
  "Return list of global minor modes."
  (seq-filter (pcase-lambda (`(,_fn ,_var ,global . _rest)) global)
              (tray-builder-minor-modes)))

(defun tray-builder-non-global-minor-modes ()
  "Filter non-global minor modes from a list."
  (seq-filter (pcase-lambda (`(,_fn ,_var ,global . _rest))
                (not global))
              (tray-builder-minor-modes)))



(defun tray-builder-generate-shortcuts (items &optional key-fn value-fn
                                              used-keys)
  "Generate shortcuts from list of ITEMS.
If KEY-FN is nil, ITEMS should be list of strings or symbols.
If KEY-FN is a function, it will be called with every item of list, and should
return string that will be as basis for shortcut.
If VALUE-FN is nil, result is an alist of generated keys and corresponding
items.
If VALUE-FN is non nil, return a list of results of calling VALUE-FN with two
arguments - generated shortcut and item.
USED-KEYS is a list of keys that shouldn't be used."
  (let* ((value-fn (or value-fn (lambda (key value)
                                  (if (proper-list-p value)
                                      (append (list key) value)
                                    (cons key value)))))
         (total (length items))
         (random-variants (append
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "a")
                                                    (string-to-char
                                                     "z")))
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "A")
                                                    (string-to-char
                                                     "Z")))))
         (variants-len (length random-variants))
         (min-len
          (if used-keys
              (length (car (seq-sort-by #'length #'> used-keys)))
            (cond ((>= variants-len total)
                   1)
                  ((>= variants-len (/ total 2))
                   2)
                  (t 3)))))
    (let ((shortcuts used-keys)
          (used-words '())
          (all-keys (mapcar (lambda (def)
                              (if key-fn
                                  (funcall key-fn def)
                                (if (symbolp def)
                                    (symbol-name def)
                                  def)))
                            items))
          (result))
      (dotimes (i (length items))
        (when-let* ((def (nth i items))
                    (word (if key-fn
                              (funcall key-fn def)
                            (if (symbolp def)
                                (symbol-name def)
                              def))))
          (when (not (member word used-words))
            (push word used-words)
            (let ((short
                   (downcase
                    (substring-no-properties word 0
                                             (min min-len
                                                  (length word))))))
              (setq short (replace-regexp-in-string "[^a-z]" "" short))
              (setq short
                    (seq-find
                     (lambda (it)
                       (not
                        (seq-find (apply-partially
                                   #'string-prefix-p it)
                                  shortcuts)))
                     (append
                      (tray-builder-get-all-key-strategies word
                                                           min-len)
                      (when (= min-len 1)
                        (or (seq-remove (lambda (key)
                                          (seq-find (apply-partially
                                                     #'string-prefix-p
                                                     (downcase key))
                                                    all-keys))
                                        random-variants)
                            random-variants)))))
              (while (and
                      (< (length short) min-len))
                (setq short (concat short (number-to-string (random 10)))))
              (push short shortcuts)
              (push
               (cond ((functionp value-fn)
                      (funcall value-fn short def))
                     (t (cons short def)))
               result)))))
      (reverse result))))

(defun tray-builder-generate-key (flag &optional used-keys)
  "Generate key for option FLAG that not present in USED-KEYS."
  (let ((pred (apply-partially #'tray-builder-shortcut-pred used-keys))
        (parts (split-string flag "" t)))
    (or (seq-find
         pred
         (remove parts "-"))
        (seq-find pred parts)
        (seq-find pred
                  (seq-difference
                   '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"
                     "o"
                     "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C"
                     "D"
                     "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
                     "S"
                     "T" "U" "V" "W" "X" "Y" "Z" "1" "2" "3" "4" "5" "6" "7"
                     "8"
                     "9" "0" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-" "="
                     "["
                     "]" "{" "}" ";" "'" "\\" ":" "\"" "|" "," "." "/" "<" ">"
                     "?" "`" "~")
                   used-keys))
        "")))

(defun tray-builder-shorten-key-description (binding)
  "Return short version of BINDING."
  (when-let* ((parts
               (unless (string-match-p "mouse" binding)
                 (split-string binding nil t))))
    (let ((str (car (reverse parts))))
      (if (key-valid-p (car (reverse (split-string str "-" t))))
          (car (reverse (split-string str "-" t)))
        binding))))

(defun tray-builder-shorten-name (name shared-prefix)
  "Trim SHARED-PREFIX from NAME."
  (if (and shared-prefix
           name
           (and (string-prefix-p shared-prefix name)))
      (replace-regexp-in-string "^[-]+\\|[-]$" ""
                                (substring-no-properties name
                                                         (length
                                                          shared-prefix)))
    name))

(defun tray-builder-name-to-doc (name)
  "Capitalize and remove all non alphapetical chars from NAME."
  (capitalize (string-join (split-string name "[^a-z]+" t) " ")))

(defun tray-builder-find-longest-prefix (strings)
  "Return longest common prefix in STRINGS."
  (setq strings (seq-sort-by #'length '> strings))
  (seq-reduce (lambda (acc it)
                (if-let ((shared (tray-builder-shared-start acc it)))
                    (setq acc shared)
                  acc))
              strings (pop strings)))

(defun tray-builder-commands-alist-to-transient (commands &optional short-descr
                                                          generate-keys)
  "Generate transient body from COMMANDS.
GENERATE-KEYS
IF SHORT-DESCR is non nil, use short descriptions.
COMMANDS should be either list of symbols,
or alist of keys and symbols."
  (let ((used-keys)
        (line)
        (shared-prefix
         (tray-builder-find-longest-prefix
          (remove nil
                  (mapcar
                   (lambda (it)
                     (when-let ((symb
                                 (if (proper-list-p
                                      it)
                                     (seq-find
                                      #'symbolp
                                      it)
                                   (seq-find
                                    #'symbolp
                                    (list
                                     (car
                                      it)
                                     (cdr
                                      it))))))
                       (symbol-name symb)))
                   commands))))
        (result))
    (while (setq line (pop commands))
      (when-let* ((symb (if (and line
                                 (symbolp line))
                            line
                          (if (proper-list-p line)
                              (seq-find #'symbolp line)
                            (seq-find #'symbolp (list (car line)
                                                      (cdr line))))))
                  (key (if (and (stringp (car-safe line))
                                (key-valid-p (car line)))
                           (when-let ((short
                                       (if generate-keys
                                           (tray-builder-shorten-key-description
                                            (car
                                             line))
                                         (car line))))
                             (if (and short (not (member short used-keys)))
                                 short
                               (car line)))
                         (let* ((name (tray-builder-shorten-name (symbol-name
                                                                  symb)
                                                                 shared-prefix))
                                (words (remove "mode"
                                               (reverse (split-string name
                                                                      "-"
                                                                      t))))
                                (found (seq-find
                                        (lambda (it)
                                          (let ((wk (substring-no-properties
                                                     it 0 1)))
                                            (and (key-valid-p
                                                  wk)
                                                 (not
                                                  (member
                                                   wk
                                                   used-keys)))))
                                        words)))
                           (if found
                               (substring-no-properties found 0 1)
                             (tray-builder-generate-key
                              name
                              used-keys)))))
                  (doc (or (if (proper-list-p line)
                               (seq-find #'stringp (remove key line))
                             (and (not short-descr)
                                  (when-let* ((doc-str (ignore-errors (documentation
                                                                       symb)))
                                              (parts (split-string
                                                      (replace-regexp-in-string
                                                       "[.][\s\t]*$"
                                                       ""
                                                       (car
                                                        (split-string
                                                         (substring-no-properties
                                                          doc-str)
                                                         "\n" t)))
                                                      nil t)))
                                    (let ((case-fold-search nil))
                                      (mapconcat
                                       (lambda (it)
                                         (if (and (string-match-p "[A-Z]" it)
                                                  (not (string-match-p "[a-z]"
                                                                       it))
                                                  (> (length it) 1))
                                             (downcase it)
                                           it))
                                       parts "\s")))))
                           (tray-builder-name-to-doc
                            (tray-builder-shorten-name (symbol-name symb)
                                                       shared-prefix)))))
        (push key used-keys)
        (if (and tray-builder-transient-doc-regexp-words
                 (seq-find (lambda (i)
                             (string-match-p
                              (regexp-opt
                               tray-builder-transient-doc-regexp-words
                               'words)
                              (format "%s" i)))
                           (list symb doc)))
            (push (list key doc symb :transient t) result)
          (push (list key doc symb) result))))
    result))



(defun tray-builder-read-description (fn)
  "Read description for FN."
  (let ((doc (replace-regexp-in-string
              "\\.$"
              ""
              (or (car
                   (split-string
                    (or (ignore-errors (documentation fn))
                        (string-join (seq-drop (split-string
                                                (symbol-name fn)
                                                "[^a-z]"
                                                t)
                                               1)
                                     "\s")
                        "")
                    "\n" t))
                  ""))))
    (read-string (format "Description for %s " (symbol-name fn))
                 (when (symbolp
                        fn)
                   doc))))

;;;###autoload
(defun tray-builder-hydra-to-transient (begin end)
  "Copy region with hydra heads between BEGIN and END as transient commands."
  (interactive "r")
  (let ((sexps (cadar
                (read-from-string (concat "(progn ("
                                          (buffer-substring-no-properties
                                           begin end)
                                          "))"))))
        (result))
    (setq result (apply #'vector
                        (mapcar (lambda (item)
                                  (let* ((key (pop item))
                                         (fn (pop item))
                                         (str (pop item)))
                                    (list
                                     (key-description key)
                                     (or
                                      (when (stringp str)
                                        str)
                                      (if (symbolp fn)
                                          (tray-builder-read-description fn)
                                        (read-string (concat (format
                                                              "Name for (key %s) "
                                                              key)))))
                                     (cond ((and (listp fn)
                                                 (not (eq 'lambda (car-safe fn))))
                                            `(lambda ()
                                               (interactive)
                                               ,fn))
                                           (t fn)))))
                                sexps)))
    (kill-new (prin1-to-string result))
    (message "Copied")
    result))

(defun tray-builder-parse-line (line-string)
  "Try to extract key, command and description from LINE-STRING."
  (setq line-string (substring-no-properties line-string))
  (setq line-string (if (and (string-match-p "^[(]" line-string)
                             (string-match-p "[)]$" line-string))
                        (substring-no-properties line-string 1
                                                 (1-
                                                  (length
                                                   line-string)))
                      line-string))
  (when-let ((parts (split-string
                     (substring-no-properties
                      line-string)
                     nil t)))
    (let* ((key (seq-find (lambda (it)
                            (and (key-valid-p it)
                                 (not (member it '("C-c" "C-x")))))
                          parts))
           (fn
            (seq-find (apply-partially #'string-match-p
                                       "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
                      (seq-sort-by #'length '> (remove key parts))))
           (symb
            (when fn
              (intern fn)))
           (doc
            (when fn
              (or
               (when-let* ((doc-str (ignore-errors (documentation
                                                    symb)))
                           (parts (split-string (replace-regexp-in-string
                                                 "[.][\s\t]*$" ""
                                                 (car (split-string
                                                       (substring-no-properties
                                                        doc-str)
                                                       "\n" t)))
                                                nil t)))
                 (let ((case-fold-search nil))
                   (mapconcat
                    (lambda (it)
                      (if (and (string-match-p "[A-Z]" it)
                               (not (string-match-p "[a-z]" it))
                               (> (length it) 1))
                          (downcase it)
                        it))
                    parts "\s")))
               (when-let ((rest (remove fn (remove key parts))))
                 (string-join rest " "))
               fn))))
      (when symb
        (list key
              doc
              symb)))))

(defun tray-builder-prettify-vector (result)
  "Return pretty string for vector RESULT."
  (with-temp-buffer
    (let ((emacs-lisp-mode-hook nil))
      (emacs-lisp-mode)
      (insert
       (prin1-to-string result)))
    (goto-char (point-min))
    (forward-char 1)
    (while (tray-builder-elisp-forward-sexp)
      (when (and (looking-back "[)]" 0)
                 (not (looking-at "[\s\t\n]*\\]" 0)))
        (newline-and-indent)))
    (font-lock-ensure)
    (buffer-string)))

(defun tray-builder-prettify-vectors (vectors)
  "Return pretty string from VECTORS."
  (if (seq-find #'vectorp (if (listp vectors)
                              vectors
                            (append vectors nil)))
      (let ((strs))
        (dolist (vect (if (listp vectors)
                          vectors
                        (append vectors nil)))
          (push (tray-builder-prettify-vector vect) strs))
        (string-join (reverse strs) "\n"))
    (tray-builder-prettify-vector (if (vectorp vectors)
                                      vectors
                                    (apply #'vector vectors)))))

(defun tray-builder--from-region-lines (beg end)
  "Generate body for transient prefix from region between BEG and END."
  (when-let* ((lines
               (when (and beg end)
                 (split-string (buffer-substring-no-properties
                                beg
                                end)
                               "[\n]" t)))
              (splitted (remove nil
                                (mapcar
                                 #'tray-builder-parse-line
                                 lines))))
    (let ((used-keys)
          (result)
          (line))
      (while (setq line (pop splitted))
        (let ((key (or (car line)
                       (let* ((name (symbol-name (car (last line))))
                              (words (reverse (split-string name
                                                            "-"
                                                            t)))
                              (found (seq-find
                                      (lambda (it)
                                        (let ((wk (substring-no-properties
                                                   it 0 1)))
                                          (and (key-valid-p
                                                wk)
                                               (not
                                                (member
                                                 wk
                                                 used-keys)))))
                                      words)))
                         (if found
                             (substring-no-properties found 0 1)
                           (tray-builder-generate-key
                            name
                            used-keys))))))
          (push key used-keys)
          (setcar line key)
          (push line result)))
      (apply #'vector (reverse
                       result)))))

(defun tray-builder--alistp (list)
  "Return T if LIST is an association list."
  (and (proper-list-p list)
       (seq-every-p #'consp list)))

(defun tray-builder-read-top-level-lists ()
  "Return all Lisp lists at outermost position in current buffer.
An \"outermost position\" means one that it is outside of any syntactic entity:
outside of any parentheses, comments, or strings encountered in the scan."
  (let ((sexps)
        (sexp))
    (goto-char (point-min))
    (while (setq sexp (ignore-errors (read (current-buffer))))
      (push sexp sexps))
    (reverse sexps)))

(defun tray-builder--from-region (beg end)
  "Extract commands from a region to build a transient.

Argument BEG is the beginning position of the region.

Argument END is the ending position of the region.

Generate a vector representing a transient command structure based on the region
between BEG and END."
  (let* ((sexps (save-excursion
                  (save-restriction
                    (narrow-to-region beg end)
                    (tray-builder-read-top-level-lists))))
         (result
          (pcase sexps
            ((pred tray-builder--alistp)
             (apply #'vector (tray-builder-commands-alist-to-transient sexps)))
            ((guard (tray-builder--alistp (car-safe sexps)))
             (apply #'vector
                    (tray-builder-commands-alist-to-transient (car-safe sexps))))
            (_ (tray-builder--from-region-lines beg end)))))
    (tray-builder-copy-as-string
     result)))

(defun tray-builder--substitute-map (sym &optional full shadow prefix title
                                         with-menu transl always-title
                                         mention-shadow buffer)
  "Return a description of the key bindings in SYM.
This is followed by the key bindings of all maps reachable
through STARTMAP.

If FULL is non-nil, don't omit certain uninteresting commands
\(such as `undefined').

If SHADOW is non-nil, it is a list of maps; don't mention keys
which would be shadowed by any of them.

If PREFIX is non-nil, mention only keys that start with PREFIX.
IF WITH-MENU is non-nil, include menu items.
If TITLE is non-nil, is a string to insert at the beginning.
TITLE should not end with a colon or a newline; we supply that.

If NOMENU is non-nil, then don't omit menu-bar commands.

If TRANSL is non-nil, the definitions are actually key
translations so print strings and vectors differently.

If ALWAYS-TITLE is non-nil, print the title even if there are no
maps to look through.

If MENTION-SHADOW is non-nil, then when something is shadowed by
SHADOW, don't omit it; instead, mention it but say it is
shadowed.

If BUFFER, lookup keys while in that buffer.  This only affects
things like :filters for menu bindings."
  (when-let ((value
              (cond ((keymapp sym)
                     sym)
                    ((stringp sym)
                     (ignore-errors (symbol-value (intern-soft sym))))
                    ((symbolp sym)
                     (symbol-value sym))))
             (buff (or buffer (current-buffer))))
    (with-temp-buffer
      (funcall (with-no-warnings
                 (if (fboundp 'help--describe-map-tree)
                     #'help--describe-map-tree
                   #'describe-map-tree))
               value
               (not full)
               shadow
               prefix
               title
               (not with-menu)
               transl
               always-title
               mention-shadow
               buff)
      (buffer-string))))

(defun tray-builder-keymap-to-alist (keymap &optional filter &rest args)
  "Format KEYMAP to alist.
FILTER is called with key description and symbol.
ARGS is the argument for `tray-builder--substitute-map'."
  (when-let* ((lines (ignore-errors (split-string (apply
                                                   #'tray-builder--substitute-map
                                                   (append
                                                    (list
                                                     keymap)
                                                    args))
                                                  "\n"
                                                  t)))
              (filtered (delq nil (mapcar
                                   (lambda
                                     (it)
                                     (when-let* ((chars 	(and it
                                                               (stringp it)
                                                               (split-string it
                                                                             "" t)))
                                                 (key-chars
                                                  (seq-take-while
                                                   (lambda (c)
                                                     (get-text-property
                                                      0
                                                      'face
                                                      c))
                                                   chars))
                                                 (key (string-join key-chars ""))
                                                 (cmd (intern-soft
                                                       (string-trim (substring-no-properties
                                                                     it (length
                                                                         key))))))
                                       (when (and (key-valid-p key)
                                                  (not (string-match-p
                                                        tray-builder-assist-exclude-regexps
                                                        key))
                                                  (not
                                                   (memq cmd
                                                         tray-builder-exclude-cmds))
                                                  (or (not filter)
                                                      (funcall filter key cmd)))
                                         (cons													(substring-no-properties
                                                                         key)
                                                                        cmd))))
                                   lines))))
    (seq-sort-by
     (lambda (a)
       (length (car a)))
     #'<
     (seq-uniq filtered (lambda (a b)
                          (eq (cdr a)
                              (cdr b)))))))



(defun tray-builder-all-keymaps (&optional filter)
  "Return alist of all keymaps filtered with FILTER."
  (let (maps)
    (mapatoms (lambda (sym)
                (when-let ((value (and (boundp sym)
                                       (keymapp (symbol-value
                                                 sym))
                                       (tray-builder-keymap-to-alist
                                        sym))))
                  (when (or (not filter)
                            (funcall filter value))
                    (push (cons sym value) maps)))))
    maps))

(defun tray-builder-get-major-modes ()
  "Return all modes from `auto-mode-alist'."
  (seq-filter #'commandp (seq-uniq (flatten-list (mapcar #'cdr auto-mode-alist)))))


(defun tray-builder-get-minor-modes-commands (&optional active)
  "Generate prefixes from `minor-mode-map-alist'.
If ACTIVE is non nil, return bodies only for active modes."
  (let ((active-modes (seq-filter #'symbol-value (mapcar #'car
                                                         minor-mode-alist))))
    (append (remove nil
                    (mapcar
                     (lambda (it)
                       (when (or (not active)
                                 (memq (car it) active-modes))
                         (when-let ((val (tray-builder-format-keymap-to-alist
                                          (cdr it))))
                           (apply #'vector
                                  :description (capitalize
                                                (symbol-name (car it)))
                                  (tray-builder-commands-alist-to-transient
                                   val)))))
                     minor-mode-map-alist))
            (list (apply #'vector :description "Local"
                         (tray-builder-commands-alist-to-transient
                          (seq-filter (lambda (it)
                                        (commandp
                                         (cdr it)))
                                      (tray-builder-format-keymap-to-alist
                                       (current-local-map)))))))))

;;;###autoload
(defun tray-builder-kill-from-commands (commands)
  "Generate transient body from COMMANDS."
  (interactive (list (mapcar #'intern (completing-read-multiple
                                       "Command "
                                       (let (maps)
                                         (mapatoms (lambda (sym)
                                                     (and (fboundp sym)
                                                          (commandp sym)
                                                          (push sym maps))))
                                         maps)))))
  (if-let ((commands (tray-builder-commands-alist-to-transient
                      commands)))
      (progn (kill-new (tray-builder-prettify-vectors commands))
             (message "Copied"))
    (message "Couldn't extract commands")))


;;;###autoload
(defun tray-builder-kill-commands-from-keymap (keymap)
  "Generate transient body from KEYMAP."
  (interactive (list (symbol-value
                      (intern (completing-read
                               "Keymap: "
                               (let (maps)
                                 (mapatoms (lambda (sym)
                                             (and (boundp sym)
                                                  (keymapp (symbol-value
                                                            sym))
                                                  (push sym maps))))
                                 maps))))))
  (if-let ((commands (reverse (tray-builder-commands-alist-to-transient
                               (tray-builder-format-keymap-to-alist keymap)
                               (yes-or-no-p "Use short descriptions?")))))
      (progn (kill-new (tray-builder-prettify-vectors commands))
             (message "Copied"))
    (message "Couldn't extract commands")))

;;;###autoload
(defun tray-builder-kill-from-minor-modes ()
  "Copy body for `transient-define-prefix' from all minor modes."
  (interactive)
  (kill-new
   (tray-builder-prettify-vectors (tray-builder-get-minor-modes-commands)))
  (message "Copied"))

;;;###autoload
(defun tray-builder-kill-from-active-minor-modes ()
  "Copy body for `transient-define-prefix' from currently active minor modes."
  (interactive)
  (let ((cmds (tray-builder-get-minor-modes-commands t)))
    (pp cmds)
    (kill-new
     (tray-builder-prettify-vectors cmds))
    (message "Copied")))

;;;###autoload
(defun tray-builder-kill-from-region-lines (&optional beg end)
  "Generate body for transient prefix from region lines between BEG and END."
  (interactive "r")
  (tray-builder--from-region
   (or
    beg
    (region-beginning))
   (or end (region-end))))

(defun tray-builder-get-local-commands ()
  "Return prefixes from `minor-mode-map-alist'.
If ACTIVE is non nil, return bodies only for active modes."
  (tray-builder-commands-alist-to-transient
   (seq-filter (lambda (it)
                 (commandp
                  (cdr it)))
               (tray-builder-format-keymap-to-alist
                (current-local-map)))))

(defun tray-builder-take-description (item)
  "Return description from transient ITEM."
  (when (proper-list-p item)
    (if (stringp (nth 1 item))
        (nth 1 item)
      (when-let ((d (car (seq-drop
                          (memq
                           :description
                           item)
                          1))))
        (if (functionp d)
            (funcall d)
          d)))))

(defun tray-builder-take-key (item)
  "Return key from transient ITEM."
  (when (stringp (car-safe item))
    (car item)))

(defun tray-builder-group-vectors (arguments &optional height win-width)
  "Group ARGUMENTS into vector.
Default value for HEIGHT is `max-mini-window-height',
and for WIN-WIDTH - window width."
  (let* ((descriptions
          (sort
           (mapcar #'(lambda
                       (&rest args)
                       (length
                        (apply
                         #'(lambda
                             (&rest args)
                             (apply #'concat
                                    (list
                                     (apply
                                      #'tray-builder-take-key
                                      args)
                                     (apply
                                      #'tray-builder-take-description
                                      args))))
                         args)))
                   arguments)
           #'>))
         (longest (+ 10 (car descriptions)))
         (win-width (or win-width (window-width)))
         (max-cols (/ win-width longest))
         (count (length arguments))
         (height (or height
                     (floor (* max-mini-window-height 100))))
         (cols (if (>= height count)
                   1
                 (/ win-width longest)))
         (final-cols (min max-cols cols))
         (final (/ count final-cols)))
    (mapcar
     (lambda (it)
       (apply #'vector it))
     (seq-split arguments final))))


(defun tray-builder-map-modes-to-prefixes (modes)
  "Map MODES to prefix shortcuts.

Argument MODES is a list of modes to map to prefixes."
  (let* ((maxwidth
          (min (length
                (seq-sort-by (pcase-lambda (`(,mode . ,_rest))
                               (length (replace-regexp-in-string
                                        "^Toggle[\s]\\|\\.$\\|-\\|Mode"
                                        (lambda (it)
                                          (pcase it
                                            ("-" " ")
                                            (_ "")))
                                        (capitalize (symbol-name mode)))))
                             #'>
                             modes))
               (window-width)))
         (all-modes
          (tray-builder-generate-shortcuts
           modes
           (lambda (it)
             (symbol-name (car it)))
           (lambda (key cell)
             (pcase-let ((`(,fn ,var ,_global ,_enabled)
                          cell))
               (list
                key
                fn
                :description
                `(lambda ()
                   (let* ((mode ',var)
                          (label (replace-regexp-in-string
                                  "^Toggle[\s]\\|\\.$\\|-\\|Mode"
                                  (lambda (it)
                                    (pcase it
                                      ("-" " ")
                                      (_ "")))
                                  (capitalize (symbol-name mode))))
                          (doc (truncate-string-to-width
                                label
                                ,maxwidth nil nil nil ".")))
                     (if (and (boundp mode)
                              (symbol-value mode))
                         (propertize doc 'face 'success)
                       doc)))
                :transient t)))))
         (groupped (mapcar
                    (lambda (it)
                      (apply #'vector it))
                    (seq-split all-modes (/ (length all-modes) 5)))))
    groupped))




;;;###autoload (autoload 'tray-builder-eval-toggle-minor-mode-prefix "tray-builder" nil t)
(transient-define-prefix tray-builder-eval-toggle-minor-mode-prefix ()
  "Toggle minor modes with dynamic shortcuts."
  [:setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       transient--prefix)
      (list (list "g" "global modes"
                  (let ((groupped (tray-builder-map-modes-to-prefixes
                                   (tray-builder-global-minor-modes)))
                        (sym (make-symbol
                              "tray-builder-toggle-global-modes")))
                    (tray-builder-eval-dynamic-eval
                     sym
                     `([,@groupped]))
                    sym))
            (list "l" "local modes"
                  (let ((groupped (tray-builder-map-modes-to-prefixes
                                   (tray-builder-non-global-minor-modes)))
                        (sym (make-symbol
                              "tray-builder-toggle-local-modes")))
                    (tray-builder-eval-dynamic-eval
                     sym
                     `([,@groupped]))
                    sym)))))])


;;;###autoload
(defun tray-builder-dwim ()
  "Show menu with relevant local commands and their real keybindings."
  (interactive)
  (call-interactively (tray-builder-eval-dynamic-eval
                       "tray-builder-active-modes"
                       `([,@(tray-builder-group-vectors
                             (tray-builder-commands-alist-to-transient
                              (append
                               (or
                                (tray-builder-keymap-to-alist
                                 (current-local-map)
                                 (lambda (_key value)
                                   (not
                                    (eq value
                                        'tray-builder-dwim))))
                                (when-let
                                    ((sym
                                      (tray-builder-help-fns--most-relevant-active-keymap)))
                                  (tray-builder-keymap-to-alist
                                   sym)))
                               (list (cons "?"
                                           'describe-mode)))
                              t))]))))


(defun tray-builder-eval-dynamic-eval (name body)
  "Eval and call transient prefix with NAME and BODY."
  (when (stringp name)
    (setq name (make-symbol name)))
  (eval `(progn
           (transient-define-prefix ,name ()
             ,@body
             (interactive)
             (transient-setup ',name))
           ',name)
        t)
  name)


;;;###autoload (autoload 'tray-builder-menu "tray-builder" nil t)
(transient-define-prefix tray-builder-menu ()
  "Generate transient prefixes from region or with commands."
  ["Generate prefix commands from "
   ("k" "Keymap" tray-builder-kill-commands-from-keymap)
   ("r" "Region lines" tray-builder-kill-from-region-lines
    :inapt-if-not region-active-p
    :transient nil)
   ("m" "Active minor modes"
    tray-builder-kill-from-active-minor-modes)
   ("A" "All minor modes"
    tray-builder-kill-from-minor-modes)]
  (interactive)
  (transient-setup #'tray-builder-menu)
  (when-let ((string-result
              (when (region-active-p)
                (tray-builder--from-region
                 (region-beginning)
                 (region-end)))))
    (momentary-string-display (concat "\n" string-result "\n")
                              (save-excursion
                                (forward-line)
                                (point))
                              nil
                              "Copied transient commands")))

(provide 'tray-builder)
;;; tray-builder.el ends here