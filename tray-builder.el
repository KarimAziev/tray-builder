;;; tray-builder.el --- Configure hydra builder -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ohai-emacs
;; Keywords: lisp
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1") (transient "0.3.7.50-git"))

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

(defun tray-builder-group-with (fn items &optional transform-fn)
  "Group ITEMS by calling FN with every item.
FN should return key.
TRANSFORM-FN should return transformed item."
  (seq-reduce (lambda (acc it)
                (let* ((key (funcall fn it))
                       (val (if transform-fn (funcall transform-fn it) it))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell)
                                          (list val))
                                (list val))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              (seq-copy items) '()))

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

(defun tray-builder--keymap-keys (keymap)
  "Return all the keys and commands in KEYMAP.
Flattens nested keymaps and follows remapped commands.

Returns a list of pairs (KEYCODES COMMAND), where KEYCODES is a
vector suitable for `key-description', and COMMAND is a smbol."
  (when keymap
    (cond ((and
            (symbolp keymap)
            (fboundp keymap)
            (keymapp (symbol-function keymap)))
           (tray-builder--keymap-keys (symbol-function keymap)))
          ((or
            (symbolp keymap)
            (functionp keymap)
            (stringp keymap)
            (vectorp keymap))
           `(([] ,keymap)))
          ((stringp (car-safe keymap))
           (tray-builder-keymap-keys-to-alist (cdr-safe keymap)))
          ((listp (cdr-safe keymap))
           (let (result)
             (dolist (item (cdr keymap))
               (cond ((and (consp item)
                           (eq (car-safe item) 'menu-bar))
                      nil)
                     ((consp item)
                      (pcase-let (()))
                      (let* ((source-0-- item)
                             (keycode
                              (car-safe
                               (prog1 source-0--
                                 (setq source-0--
                                       (cdr source-0--)))))
                             (value source-0--))
                        (mapc
                         #'(lambda
                             (input0)
                             (let* ((source-1-- input0)
                                    (keycodes
                                     (car-safe
                                      (prog1 source-1--
                                        (setq source-1--
                                              (cdr source-1--)))))
                                    (command
                                     (car source-1--)))
                               (setq result
                                     (cons
                                      (list
                                       (vconcat
                                        (vector keycode)
                                        keycodes)
                                       command)
                                      result))))
                         (tray-builder--keymap-keys value))))
                     ((char-table-p item)
                      (map-char-table
                       (lambda (keycode value)
                         (mapc
                          #'(lambda
                              (input0)
                              (let* ((source-14-- input0)
                                     (keycodes
                                      (car-safe
                                       (prog1 source-14--
                                         (setq source-14--
                                               (cdr source-14--)))))
                                     (command
                                      (car source-14--)))
                                (setq result
                                      (cons
                                       (list
                                        (vconcat
                                         (vector keycode)
                                         keycodes)
                                        command)
                                       result))))
                          (tray-builder--keymap-keys value)))
                       item))))
             (setq result
                   (let (res)
                     (let ((list result)
                           (i 0))
                       (while list
                         (let ((it
                                (car-safe
                                 (prog1 list
                                   (setq list
                                         (cdr list)))))
                               (it-index i))
                           (ignore it it-index)
                           (setq res
                                 (cons
                                  (if
                                      (let ((input0 it))
                                        (let* ((source-20-- input0)
                                               (keycodes
                                                (car-safe
                                                 (prog1 source-20--
                                                   (setq source-20--
                                                         (cdr
                                                          source-20--)))))
                                               (_
                                                (car source-20--)))
                                          (and
                                           (>
                                            (length keycodes)
                                            1)
                                           (eq
                                            (elt keycodes 0)
                                            'remap))))
                                      (let ((input0 it))
                                        (let* ((source-19-- input0)
                                               (keycodes
                                                (car-safe
                                                 (prog1 source-19--
                                                   (setq source-19--
                                                         (cdr
                                                          source-19--)))))
                                               (command
                                                (car source-19--)))
                                          (list
                                           (where-is-internal
                                            (elt keycodes 1)
                                            global-map t)
                                           command)))
                                    it)
                                  res)))
                         (setq i
                               (1+ i))))
                     (nreverse res)))
             (nreverse result))))))

(defun tray-builder--key-description (key)
  "Return key description for KEY without errors."
  (or
   (ignore-errors (key-description
                   (kbd key)))
   (ignore-errors (key-description key))
   (ignore-errors (key-description (char-to-string
                                    key)))
   (ignore-errors
     (key-description (vector key)))))

(defun tray-builder-keymap-keys-to-alist (keymap &optional filter)
  "Map KEYMAP to alist with FILTER function.
FILTER function will be called with two arguments - key description and value."
  (let ((exclude-cmds '(digit-argument negative-argument
                                       self-insert-command
                                       undefined)))
    (delq nil
          (mapcar (lambda (it)
                    (when (listp it)
                      (when-let ((key-descr
                                  (tray-builder--key-description (car
                                                                  it)))
                                 (value (if (listp (cdr it))
                                            (cadr it)
                                          (cdr it))))
                        (unless (or
                                 (string-empty-p key-descr)
                                 (memq (cadr it) exclude-cmds)
                                 (string-match-p
                                  "\\(^<\\|keymap\\|follow-link\\|compose-last-chars\\|drag-n-drop\\|menu\\|XF86Forward\\|XF86Back\\|help\\|iconify-frame\\|touch\\|mouse\\|wheel\\)\\|\\.\\."
                                  key-descr)
                                 (byte-code-function-p value)
                                 (when filter
                                   (not (funcall filter key-descr value))))
                          (cons key-descr value)))))
                  (reverse (tray-builder--keymap-keys (keymap-canonicalize
                                                       keymap)))))))

(defun tray-builder-format-keymap-to-alist (keymap &optional symb-prefix)
  "Convert KEYMAP to alist and filter by SYMB-PREFIX."
  (when (keymapp keymap)
    (if-let ((name (when symb-prefix
                     (car
                      (split-string (if (symbolp symb-prefix)
                                        (symbol-name symb-prefix)
                                      symb-prefix)
                                    "-" t)))))
        (tray-builder-keymap-keys-to-alist keymap
                                             (lambda (_k v)
                                               (and (symbolp v)
                                                    (string-prefix-p name
                                                                     (symbol-name
                                                                      v)))))
      (tray-builder-keymap-keys-to-alist keymap))))

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
  "Generate preffered keybinding from WORD with length LEN."
  (let* ((parts (append (split-string word "[^a-zz-a]" t)
                        (list (replace-regexp-in-string "[^a-zz-a]" "" word))))
         (parts-len (length parts))
         (finalize (lambda (short)
                     (while (> len (length short))
                       (setq short (concat short (number-to-string (random 10)))))
                     (tray-builder-safe-substring len short)))
         (vars
          (mapcar finalize (tray-builder-capitalize-variants
                            (tray-builder-safe-substring len
                                                         (replace-regexp-in-string
                                                          "[^a-zz-a]"
                                                          ""
                                                          word))))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (tray-builder-shared-start word it)))))
     '>
     (seq-uniq (append
                vars
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat (apply-partially
                                                 'tray-builder-safe-substring n)
                                                parts)))
                 (number-sequence 1 (min len parts-len)))
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat (apply-partially
                                                 'tray-builder-safe-substring n)
                                                (reverse parts))))
                 (number-sequence 1 (min len parts-len))))))))

(defun tray-builder-generate-shortcuts (words max-len)
  "Generate shortcuts from WORDS with a given MAX-LEN."
  (let ((shortcuts '())
        (used '())
        (result))
    (dolist (sym words)
      (let ((word (if (symbolp sym)
                      (symbol-name sym)
                    sym)))
        (when (not (member word used))
          (let ((short
                 (downcase
                  (substring-no-properties word 0
                                           (min (1+ max-len)
                                                (length word))))))
            (setq short (replace-regexp-in-string "[^a-z]" "" short))
            (setq short
                  (seq-find (lambda (it)
                              (not (member it shortcuts)))
                            (tray-builder-get-all-key-strategies word
                                                                 (1+
                                                                  max-len))))
            (while (and (member short shortcuts))
              (setq short (if (<= (length short) max-len)
                              (upcase
                               (substring-no-properties word 0 (min (1+
                                                                     max-len)
                                                                    (length
                                                                     word))))
                            (concat short (number-to-string (random 10))))))
            (push short shortcuts)
            (push word used)
            (push (cons short sym)
                  result)))))
    result))

(defun tray-builder-generate-key (flag &optional used-keys)
  "Generate key for option FLAG that not present in USED-KEYS."
  (let ((pred (apply-partially #'tray-builder-shortcut-pred used-keys))
        (parts (split-string flag "" t)))
    (or (seq-find
         pred
         (remove parts "-"))
        (append flag)
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

(defun tray-builder-commands-alist-to-transient (commands)
  "Generate transient body from COMMANDS.
COMMANDS should be either list of symbols,
or alist of keys and symbols."
  (let ((used-keys)
        (line)
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
                           (car line)
                         (let* ((name (symbol-name symb))
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
                              used-keys)))))
                  (doc (if (proper-list-p line)
                           (seq-find #'stringp (remove key line))
                         (or (when-let* ((doc-str (ignore-errors (documentation
                                                                  symb)))
                                         (parts (split-string (replace-regexp-in-string
                                                               "[.][\s\t]?+$" ""
                                                               (car (split-string
                                                                     (substring-no-properties
                                                                      doc-str)
                                                                     "\n" t)))
                                                              nil t)))
                               (let ((case-fold-search nil))
                                 (mapconcat
                                  (lambda (it)
                                    (if (and (string-match-p "[A-ZZ-A]" it)
                                             (not (string-match-p "[a-z]" it))
                                             (> (length it) 1))
                                        (downcase it)
                                      it))
                                  parts "\s")))
                             (symbol-name symb)))))
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
    (reverse result)))

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
                                                "[^a-zZ-A]"
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
                                     (or (when (stringp str)
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
           (symb (when fn
                   (intern fn)))
           (doc (when fn
                  (or
                   (when-let* ((doc-str (ignore-errors (documentation
                                                        symb)))
                               (parts (split-string (replace-regexp-in-string
                                                     "[.][\s\t]?+$" ""
                                                     (car (split-string
                                                           (substring-no-properties
                                                            doc-str)
                                                           "\n" t)))
                                                    nil t)))
                     (let ((case-fold-search nil))
                       (mapconcat
                        (lambda (it)
                          (if (and (string-match-p "[A-ZZ-A]" it)
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
                 (not (looking-at "[\s\t\n]?+\\]" 0)))
        (newline-and-indent)))
    (buffer-substring-no-properties (point-min)
                                    (point-max))))

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
  (when-let* ((lines (when (and beg end)
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
      (setq result (apply #'vector (reverse
                                    result)))
      (tray-builder-prettify-vector result))))


(defun tray-builder-get-all-minor-modes-short-commands (&optional active)
  "Return commands from minor modes with short keybindings.
If ACTIVE is non nil, use only active modes."
  (append (remove nil
                  (mapcar
                   (lambda (it)
                     (when (or (not active)
                               (symbol-value (car it)))
                       (when-let ((val (seq-filter
                                        (lambda (it)
                                          (and
                                           (not
                                            (string-match-p "C-\\|M-"
                                                            (car
                                                             it)))
                                           (commandp (cdr it))))
                                        (tray-builder-format-keymap-to-alist
                                         (cdr it)))))
                         (apply #'vector
                                :description (capitalize
                                              (symbol-name (car it)))
                                (tray-builder-commands-alist-to-transient
                                 val)))))
                   minor-mode-map-alist))
          (list (apply #'vector :description "Local"
                       (tray-builder-commands-alist-to-transient
                        (seq-filter
                         (lambda (it)
                           (and (commandp (cdr it))
                                (not
                                 (string-match-p "C-\\|M-" (car
                                                            it)))))
                         (tray-builder-format-keymap-to-alist
                          (current-local-map))))))))

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
  (interactive (list (mapcar 'intern (completing-read-multiple
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
  (if-let ((commands (tray-builder-commands-alist-to-transient
                      (tray-builder-format-keymap-to-alist keymap))))
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
  (if-let ((string-result (tray-builder--from-region-lines
                           (or
                            beg
                            (region-beginning))
                           (or end (region-end)))))
      (progn (kill-new string-result)
             (momentary-string-display string-result
                                       (point)
                                       nil
                                       "Copied transient commands"))
    (message "No active region")))


(defun tray-builder-transient-make-toggled-description (mode &optional
                                                             description align)
  "Concat DESCRIPTION for MODE with colorized suffixes ON-LABEL and OFF-LABEL."
  (lambda ()
    (let ((mode mode)
          (description description)
          (align align))
      (setq align (or align (when description (1+ (length description)))
                      40))
      (concat
       (propertize
        (or
         description
         (when-let ((doc (replace-regexp-in-string
                          "-" " " (capitalize (symbol-name
                                               mode)))))
           (truncate-string-to-width
            (replace-regexp-in-string "^Toggle[\s]" ""
                                      (replace-regexp-in-string "\\.$" ""
                                                                (car
                                                                 (split-string
                                                                  doc
                                                                  "\n"
                                                                  nil))))
            align
            nil nil
            1)))
        'face
        (if
            (and (boundp mode)
                 (symbol-value mode))
            'success nil))
       (propertize " " 'display
                   (list 'space :align-to align))
       (if (and (boundp mode)
                (symbol-value mode))
           "[X]" "[ ]")))))

(defun tray-builder-get-local-commands ()
  "Return prefixes from `minor-mode-map-alist'.
If ACTIVE is non nil, return bodies only for active modes."
  (tray-builder-commands-alist-to-transient
   (seq-filter (lambda (it)
                 (commandp
                  (cdr it)))
               (tray-builder-format-keymap-to-alist
                (current-local-map)))))

;;;###autoload
(defun tray-builder-eval-toggle-minor-mode-prefix ()
  "Generate and call transient prefix with all minor modes."
  (interactive)
  (let* ((all-modes (tray-builder-generate-shortcuts
                     (remove nil
                             (mapcar
                              (lambda (mode)
                                (when (and (boundp mode)
                                           (commandp mode))
                                  mode))
                              minor-mode-list))
                     2))
         (max-len (+ 10 (apply 'max
                               (mapcar (lambda (it)
                                         (string-width (symbol-name
                                                        (cdr it))))
                                       all-modes))))
         (len (/ (length all-modes) 2))
         (heads))
    (setq all-modes (mapcan 'cdr
                            (tray-builder-group-with (lambda (it)
                                                       (car (split-string
                                                             (symbol-name
                                                              (cdr
                                                               it))
                                                             "-"
                                                             t)))
                                                     all-modes)))
    (dotimes (idx (length all-modes))
      (let* ((cell (nth idx all-modes))
             (mode (cdr cell))
             (key (car cell)))
        (push (list
               key
               mode
               :description
               (tray-builder-transient-make-toggled-description
                mode
                nil
                (if (>= idx len)
                    (* max-len 2)
                  max-len)))
              heads)))
    (setq heads (reverse heads))
    (setq heads (mapcar (lambda (it)
                          (apply 'vector it))
                        (seq-split heads (/ (length heads) 2))))
    (eval `(progn (transient-define-prefix
                    tray-builder--toggle-minor-mode ()
                    :transient-non-suffix 'transient--do-stay
                    :transient-suffix
                    'transient--do-call
                    ,(apply 'vector heads))
                  (tray-builder--toggle-minor-mode)))))

;;;###autoload
(defun tray-builder-eval-all-active-modes-commands ()
  "Dispatch local commands with short keys."
  (interactive)
  (eval `(progn (transient-define-prefix tray-builder-dispatch-short-keys ()
                  :transient-non-suffix 'transient--do-exit
                  ,@(tray-builder-get-minor-modes-commands t))
                (tray-builder-dispatch-short-keys))))

;;;###autoload
(defun tray-builder-eval-local-commands-prefix ()
  "Eval and call transient prefix with active minor commands."
  (interactive)
  (eval `(progn (transient-define-prefix tray-builder-dispatch-short-keys ()
                  :transient-suffix 'transient--do-stay
                  :transient-non-suffix 'transient--exit
                  ,@(tray-builder-get-all-minor-modes-short-commands t))
                (tray-builder-dispatch-short-keys))))

;;;###autoload (autoload 'tray-builder-menu "tray-builder.el" nil t)
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
  (transient-setup 'tray-builder-menu)
  (when-let ((string-result (when (region-active-p)
                              (tray-builder--from-region-lines
                               (region-beginning)
                               (region-end)))))
    (momentary-string-display string-result
                              (point)
                              nil
                              "Copied transient commands")))

(provide 'tray-builder)
;;; tray-builder.el ends here