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
  "List of words to match in transient documentation for tray-builder actions.

A list of words used to match against documentation strings for transient
commands in the tray builder. These words are typically associated with
navigation or state-changing actions such as moving through text or undoing
changes.

Each element in the list must be a string that represents a word to be matched.
The default list includes common action words like \"forward\", \"backward\",
\"undo\", and \"redo\".

The matching process uses these words to identify relevant documentation strings
for transient commands, aiding in the creation of a context-sensitive tray of
actions."
  :type '(repeat string)
  :group 'tray-builder)

(defvar tray-builder-transient-options '((:level (integer 1))
                                         (:transient (boolean))
                                         (:show-help (function))
                                         (:face (face))
                                         (:inapt-face (face))
                                         (:if (function))
                                         (:inapt-if-not-mode (symbol))
                                         (:inapt-if-mode (symbol))
                                         (:inapt-if-derived (symbol))
                                         (:inapt-if-not-derived (symbol))
                                         (:if-not-derived (symbol))
                                         (:if-not-mode (symbol))
                                         (:if-mode (symbol))
                                         (:if-derived (symbol))
                                         (:if-nil (variable))
                                         (:if-non-nil (variable))
                                         (:inapt-if-not (function))
                                         (:inapt-if-nil (variable))
                                         (:inapt-if-non-nil (variable))))

(defun tray-builder--make-toggled-description (mode &optional description align)
  "Concat DESCRIPTION for MODE with colorized suffixes ON-LABEL and OFF-LABEL."
  (lambda ()
    (concat
     (propertize
      (or
       description
       (when-let ((doc (replace-regexp-in-string
                        "-" " " (capitalize (symbol-name
                                             mode)))))
         (replace-regexp-in-string "\\.$" ""
                                   (car
                                    (split-string doc "\n" nil)))))
      'face
      (if
          (and (boundp mode)
               (symbol-value mode))
          'success nil))
     (propertize " " 'display
                 (list 'space :align-to (or align 32)))
     (if (and (boundp mode)
              (symbol-value mode))
         "[X]" "[ ]"))))

(defcustom tray-builder-align-toggle-num 35
  "Number of spaces for alignment in tray builder toggles.

Specifies the number of characters to use for alignment when toggling display
options in the tray builder.

The value is an integer that determines the column width for aligning toggle
descriptions. This alignment affects how toggle descriptions are displayed when
the tray builder generates toggle suffixes.

Increasing the value will result in more space between the toggle key and its
description, while decreasing the value will reduce the space. Adjust this value
to achieve the desired visual alignment in the tray builder's display.

The default value is 35. Adjust this value according to the width of the window
or personal preference for the appearance of the tray builder's toggle display."
  :group 'tray-builder
  :type 'integer)

(defcustom tray-builder-toggle-suffixes `((display-line-numbers-mode
                                           :key "l")
                                          (visual-line-mode :key "v")
                                          (whitespace-mode :description
                                           "Show Whitespaces")
                                          (dimmer-extra-transient
                                           :description
                                           "Highlight selected buffers"
                                           :if-require (dimmer-extra))
                                          (repeat-mode)
                                          (visual-fill-column-mode
                                           :description
                                           "Visual Fill Column")
                                          (page-break-lines-mode
                                           :description
                                           "Hide ^L characters")
                                          (rainbow-mode :description
                                           "Highlight colors")
                                          (hl-line-mode
                                           :description
                                           "Highlight the current line")
                                          (hl-todo-mode :description
                                           "Highlight TODO words")
                                          (toggle-truncate-lines
                                           :key "t"
                                           :description
                                           "Truncate long lines"
                                           :variable-indicator truncate-lines)
                                          (marginalia-mode :key "A")
                                          (minibuffer-auto-mode :key "M")
                                          (minibuffer-auto-preview-mode :key
                                           "R")
                                          (minibuffer-auto-crm-mode :key "m")
                                          (icomplete-mode :key "I")
                                          (fido-mode :key "F")
                                          (fido-vertical-mode :key "V")
                                          (ivy-mode :key "i")
                                          (counsel-mode :if-require (counsel))
                                          (auto-fill-mode :description
                                           ,(tray-builder--make-toggled-description
                                             'auto-fill-function
                                             (concat "Auto Wrapping at column "
                                              (propertize
                                               (format
                                                "%s"
                                                fill-column)
                                               'face
                                               'font-lock-keyword-face))
                                             (* tray-builder-align-toggle-num 2)))
                                          (emmet-mode :key "E")
                                          (toggle-debug-on-error
                                           :key "d"
                                           :variable-indicator debug-on-error
                                           :description
                                           "Debug on error")
                                          (toggle-debug-on-quit
                                           :key "X"
                                           :variable-indicator debug-on-quit)
                                          (transient-toggle-debug
                                           :key "O"
                                           :variable-indicator transient--debug)
                                          (treesit-inspect-mode
                                           :if
                                           (lambda ()
                                             (fboundp 'treesit-inspect-mode))
                                           :description
                                           (lambda () "Tree sit inspect")
                                           :inapt-if-not (lambda ()
                                                           (derived-mode-p
                                                            'c-ts-mode
                                                            'cmake-ts-mode
                                                            'cpp-ts-mode
                                                            'css-ts-mode
                                                            'dockerfile-ts-mode
                                                            'elixir-ts-mode
                                                            'go-ts-mode
                                                            'html-ts-mode
                                                            'java-ts-mode
                                                            'tsx-ts-mode
                                                            'typescript-ts-mode
                                                            'json-ts-mode
                                                            'julia-ts-mode
                                                            'kotlin-ts-mode
                                                            'python-ts-mode
                                                            'ruby-ts-mode
                                                            'rust-ts-mode
                                                            'toml-ts-mode
                                                            'yaml-ts-mode
                                                            'json-ts-mode
                                                            'c++-ts-mode)))
                                          (helm-mode :if-require (helm))
                                          (org-toggle-inline-images
                                           :if-derived
                                           org-mode
                                           :variable-indicator
                                           org-inline-image-overlays))
  "List of toggle commands with optional keys and descriptions.

A list of toggle commands and their associated properties for use in the tray
builder interface. Each entry in the list is a cons cell with the toggle command
as the car and a property list as the cdr.

The property list can contain the following keys:

- :key - A string representing a single-character keybinding for the toggle
command.
- :description - A string or a function that returns a string, providing a
  description for the toggle command.
- :variable-indicator - A symbol representing a variable that indicates the
  toggle state.
- :if-require - A list of symbols representing required features for the toggle
  command to be available.
- :if-derived - A symbol representing a major mode that the current buffer must
  be derived from for the toggle to be applicable.
- :inapt-if-not - A function that determines if the toggle command is
  inapplicable based on thea current buffer's context.

Entries without a :key will not have a keybinding in the tray builder interface.
Entries without a :description will use the default description generated by the
tray builder. If :variable-indicator is provided, its value will be used to
indicate the toggle state instead of the command's return value. If :if-require
is provided, the toggle command will only be available if all listed features
are loaded. If :if-derived is provided, the toggle will only be applicable if
the current buffer is derived from the specified major mode. If :inapt-if-not is
provided, the function will be called to determine the applicability of the
toggle command.

The default value is an alist where each element specifies a toggle command and
its associated properties for customization."
  :group 'tray-builder
  :type
  `(alist
    :key-type (symbol :tag "Command" ignore)
    :value-type
    (plist
     :options
     (((const
        :format "%v "
        :description)
       (radio
        (string)
        (function ignore)))
      ((const
        :format "%v "
        :variable-indicator)
       (variable))
      ((const
        :format "%v "
        :if-require)
       (repeat symbol))
      ((const
        :format "%v "
        :key)
       (string))
      ,@(mapcar (lambda (it)
                  `((const
                     :format "%v "
                     ,(car it))
                    ,(cadr it)))
         tray-builder-transient-options)))))

(defun tray-builder--plist-pick (keywords pl)
  "Extract specified keys and values from a property list.

Argument KEYWORDS is a list of keys to pick from the property list.

Argument PL is the property list from which values associated with KEYWORDS are
picked."
  (let ((result)
        (keyword))
    (while (setq keyword (pop keywords))
      (when (plist-member pl keyword)
        (let ((value (plist-get pl keyword)))
          (setq result (append result (list keyword value))))))
    result))

(defvar tray-builder-exclude-cmds
  '(ignore
    self-insert-command
    digit-argument
    undefined
    scroll-up-command
    scroll-down-command
    negative-argument)
  "List of commands to exclude from tray building.")

(defvar tray-builder-assist-exclude-regexps
  "\\(^<\\|keymap\\|follow-link\\|compose-last-chars\\|drag-n-drop\\|menu\\|XF86Forward\\|XF86Back\\|help\\|iconify-frame\\|touch\\|mouse\\|wheel\\)\\|\\.\\."
  "List of regex patterns to exclude from tray builder assist.")

(defun tray-builder-help-fns-find-keymap-name (keymap)
  "Find a symbol name for a given keymap.

Argument KEYMAP is a keymap to search for its variable name."
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
  "Find the most relevant active keymap at point."
  (tray-builder-help-fns-find-keymap-name (or (get-char-property (point) 'keymap)
                         (if (get-text-property (point) 'local-map)
                             (get-char-property (point) 'local-map)
                           (current-local-map)))))

(defun tray-builder-copy-as-string (result)
  "Copy formatted RESULT to clipboard and display it.

Argument RESULT is the data structure to be converted to a string and copied to
the clipboard."
  (let ((content (tray-builder-prettify-vector result)))
    (kill-new content)
    (message content)
    content))

(defun tray-builder-shared-start (s1 s2)
  "Find common prefix of strings S1 and S2.

Argument S1 is a string to compare.

Argument S2 is another string to compare against S1."
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
  "Convert KEYMAP to alist, optionally filtering with symb-prefix.

Argument KEYMAP is the keymap to convert to an alist.

Optional argument SYMB-PREFIX is a symbol or string used as a prefix to filter
KEYMAP entries."
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
  "Check if KEY is valid and not in USED-KEYS.

Argument USED-KEYS is a list of keys that have already been used.

Argument KEY is the key to be checked for validity and non-membership in
USED-KEYS."
  (and
   (key-valid-p key)
   (not (member key used-keys))))

(defun tray-builder-move-with (fn &optional n)
  "Move point using FN, optionally N times.

Argument FN is a function to be called that moves the point.

Optional argument N is the number of times to call FN; it defaults to 1."
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
  "Move forward over a balanced expression.

Optional argument ARG is the numeric argument specifying the number of sexps to
move forward; it defaults to 1."
  (tray-builder-move-with 'forward-sexp arg))

(defun tray-builder-safe-substring (len word)
  "Extract a substring without properties up to length LEN.

Argument LEN is the maximum number of characters to include in the substring.

Argument WORD is the string from which the substring is extracted."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun tray-builder-capitalize-variants (word)
  "Generate capitalized variants of WORD.

Argument WORD is a string to generate capitalized variants from."
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
  "Generate key strategies from a given WORD and length.

Argument WORD is a string to be processed.

Argument LEN is an integer representing the desired length of the output
strings."
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
  "List active minor modes and their details."
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
  "List global minor modes using `tray-builder-minor-modes'."
  (seq-filter (pcase-lambda (`(,_fn ,_var ,global . _rest)) global)
              (tray-builder-minor-modes)))

(defun tray-builder-non-global-minor-modes ()
  "List non-global minor modes from `tray-builder-minor-modes'."
  (seq-filter (pcase-lambda (`(,_fn ,_var ,global . _rest))
                (not global))
              (tray-builder-minor-modes)))



(defun tray-builder-generate-shortcuts (items &optional key-fn value-fn
                                              used-keys)
  "Generate shortcuts for ITEMS using optional KEY-FN and VALUE-FN.

Argument ITEMS is a list of items to generate shortcuts for.

Optional argument KEY-FN is a function that takes an item and returns a string
to be used as the key.

Optional argument VALUE-FN is a function that takes a key and a value, and
returns a new value to be associated with the key.

Optional argument USED-KEYS is a list of strings representing keys that are
already in use and should not be generated again."
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
  "Generate a unique key based on FLAG and USED-KEYS.

Argument FLAG is a string representing the flag for which to generate a key.

Optional argument USED-KEYS is a list of strings representing keys that are
already in use and should be avoided."
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
  "Shorten key descriptions, excluding mouse bindings.

Argument BINDING is a string representing a key sequence to be shortened."
  (when-let* ((parts
               (unless (string-match-p "mouse" binding)
                 (split-string binding nil t))))
    (let ((str (car (reverse parts))))
      (if (key-valid-p (car (reverse (split-string str "-" t))))
          (car (reverse (split-string str "-" t)))
        binding))))

(defun tray-builder-shorten-name (name shared-prefix)
  "Shorten NAME by removing SHARED-PREFIX and extra hyphens.

Argument NAME is the string to be shortened.

Argument SHARED-PREFIX is the prefix to be removed from NAME if present."
  (if (and shared-prefix
           name
           (and (string-prefix-p shared-prefix name)))
      (replace-regexp-in-string "^[-]+\\|[-]$" ""
                                (substring-no-properties name
                                                         (length
                                                          shared-prefix)))
    name))

(defun tray-builder-name-to-doc (name)
  "Capitalize and join words in NAME, splitting on non-alpha characters.

Argument NAME is a string to be converted to documentation text."
  (capitalize (string-join (split-string name "[^a-z]+" t) " ")))

(defun tray-builder-find-longest-prefix (strings)
  "Find the longest common prefix in a list of STRINGS.

Argument STRINGS is a list of strings to find the longest common prefix."
  (setq strings (seq-sort-by #'length '> strings))
  (seq-reduce (lambda (acc it)
                (if-let ((shared (tray-builder-shared-start acc it)))
                    (setq acc shared)
                  acc))
              strings (pop strings)))

(defun tray-builder-commands-alist-to-transient (commands &optional short-descr
                                                          generate-keys)
  "Convert command alist to transient menu structure.

Argument COMMANDS is a list of symbols or lists representing commands.

Optional argument SHORT-DESCR is a boolean flag; when non-nil, it indicates to
use short descriptions for commands.

Optional argument GENERATE-KEYS is a boolean flag; when non-nil, it indicates to
generate keys for COMMANDS automatically."
  (let ((used-keys)
        (line)
        (shared-prefix
         (tray-builder-find-longest-prefix
          (remove nil
                  (mapcar
                   (lambda (it)
                     (when-let ((symb
                                 (pcase it
                                   ((pred (symbolp))
                                    it)
                                   ((pred (proper-list-p))
                                    (seq-find #'symbolp it))
                                   ((pred consp)
                                    (seq-find #'symbolp (list (car it)
                                                              (cdr it)))))))
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
  "Prompt for a description of FN with default from documentation.

Argument FN is the function for which to read the description."
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
  "Convert hydra to transient command vector.

Argument BEGIN is the position of the beginning of the region in the buffer.

Argument END is the position of the end of the region in the buffer."
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
  "Parse and split LINE-STRING into components.

Argument LINE-STRING is a string representing a line to be parsed."
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
  "Prettify a vector RESULT into a readable string format.

Argument RESULT is the vector to be prettified."
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
  "Prettify VECTORS or a single vector into strings.

Argument VECTORS is a list of vectors or a single vector to be prettified."
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
  "Transform region lines into a vector with unique keys.

Argument BEG is the beginning position of the region.

Argument END is the ending position of the region."
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
  "Check if LIST is a proper alist.

Argument LIST is a proper list to be checked if it is an alist."
  (and (proper-list-p list)
       (seq-every-p #'consp list)))

(defun tray-builder-read-top-level-lists ()
  "Parse and collect top-level s-expressions from buffer."
  (let ((sexps)
        (sexp))
    (goto-char (point-min))
    (while (setq sexp (ignore-errors (read (current-buffer))))
      (push sexp sexps))
    (reverse sexps)))

(defun tray-builder--from-region (beg end)
  "Convert region between BEG and END into a transient command vector.

Argument BEG is the beginning position of the region.

Argument END is the ending position of the region."
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
  "Substitute keymap symbols with their values in a temporary buffer.

Argument SYM is a keymap, a string naming a symbol, or a symbol whose value is a
keymap.

Optional argument FULL is a boolean; when non-nil, it includes inherited
keymaps.

Optional argument SHADOW is a boolean; when non-nil, it shows keys that are
shadowed by higher-precedence maps.

Optional argument PREFIX is a string or a vector of events to be used as a
PREFIX for keys in the keymap.

Optional argument TITLE is a string used as the title of the keymap description.

Optional argument WITH-MENU is a boolean; when non-nil, it includes menu
bindings.

Optional argument TRANSL is a boolean; when non-nil, it translates keys to their
ASCII equivalents.

Optional argument ALWAYS-TITLE is a boolean; when non-nil, it forces the display
of the TITLE even if it would normally be omitted.

Optional argument MENTION-SHADOW is a boolean; when non-nil, it mentions when a
binding is shadowed by another map.

Optional argument BUFFER is the buffer in which the keymap is active; defaults
to the current buffer."
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
  "Convert KEYMAP to alist, optionally filtering with FILTER and ARGS.

Argument KEYMAP is a keymap object to be converted to an alist.

Optional argument FILTER is a function that takes a key and command as arguments
and returns non-nil if the key-command pair should be included in the output.

Remaining arguments ARGS are additional arguments passed to the internal
function `tray-builder--substitute-map'."
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
  "List all keymaps, optionally filtered.

Optional argument FILTER is a predicate function to determine which keymaps to
include."
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
  "Filter and return unique major mode commands."
  (seq-filter #'commandp (seq-uniq (flatten-list (mapcar #'cdr auto-mode-alist)))))


(defun tray-builder-get-minor-modes-commands (&optional active)
  "List minor mode commands for transient display.

Optional argument ACTIVE is a boolean flag indicating whether to include only
ACTIVE minor modes. If nil, all minor modes are included."
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
  "Copy selected COMMANDS to the kill ring.

Argument COMMANDS is a list of command symbols to be processed."
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
  "Extract and copy KEYMAP commands to kill ring.

Argument KEYMAP is a symbol representing the keymap to extract commands from."
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
  "Copy minor mode commands to the kill ring."
  (interactive)
  (kill-new
   (tray-builder-prettify-vectors (tray-builder-get-minor-modes-commands)))
  (message "Copied"))

;;;###autoload
(defun tray-builder-kill-from-active-minor-modes ()
  "Copy active minor modes' commands to the kill ring."
  (interactive)
  (let ((cmds (tray-builder-get-minor-modes-commands t)))
    (pp cmds)
    (kill-new
     (tray-builder-prettify-vectors cmds))
    (message "Copied")))

;;;###autoload
(defun tray-builder-kill-from-region-lines (&optional beg end)
  "Kill lines from region to build a transient.

Optional argument BEG is the beginning position of the region; it defaults to
the current region's beginning if not provided.

Optional argument END is the ending position of the region; it defaults to the
current region's END if not provided."
  (interactive "r")
  (tray-builder--from-region
   (or
    beg
    (region-beginning))
   (or end (region-end))))

(defun tray-builder-get-local-commands ()
  "Extract commands from the current local keymap."
  (tray-builder-commands-alist-to-transient
   (seq-filter (lambda (it)
                 (commandp
                  (cdr it)))
               (tray-builder-format-keymap-to-alist
                (current-local-map)))))

(defun tray-builder-take-description (item)
  "Extract description from ITEM if present.

Argument ITEM is a proper list representing the item to take the description
from."
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
  "Extract the first element of ITEM if it's a string.

Argument ITEM is the element from which to extract the key, expected to be a
cons cell or list with a string as its first element."
  (when (stringp (car-safe item))
    (car item)))

(defun tray-builder-group-vectors (arguments &optional height win-width)
  "Group ARGUMENTS into vectors based on HEIGHT and WIN-WIDTH.

Argument ARGUMENTS is a list of items to be grouped into vectors.

Optional argument HEIGHT is the maximum number of lines for the display; it
defaults to a fraction of `max-mini-window-height'.

Optional argument WIN-WIDTH is the width of the window in characters; it
defaults to the current window width."
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
  "Map major MODES to keyboard shortcuts.

Argument MODES is a list of mode symbols to map to prefixes."
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
  "Toggle global or local minor modes dynamically."
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
  "Show a menu with relevant local commands and keybindings."
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
  "Evaluate BODY and define transient prefix NAME dynamically.

Argument NAME is a string or symbol that names the transient command.

Argument BODY is a list of forms that define the transient command."
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
  "Display menu to generate transient commands."
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


(defun tray-builder--format-menu-heading (title &optional note)
  "Format TITLE as a menu heading.
When NOTE is non-nil, append it the next line."
  (let ((no-wb (= (frame-bottom-divider-width) 0)))
    (format "%s%s%s"
            (propertize title 'face `(:inherit font-lock-constant-face
                                      :overline ,no-wb)
                        'display '((height 1.1)))
            (propertize " " 'face `(:inherit font-lock-constant-face
                                    :overline ,no-wb)
                        'display '(space :align-to right))
            (propertize (if note (concat "\n" note) "") 'face
                        'font-lock-doc-face))))

(defvar tray-builder--boolean-variable-history nil)

;;;###autoload
(defun tray-builder-toggle-custom-boolean-var (sym)
  "Toggle the boolean value of a given symbol.

Argument SYM is a symbol whose value is to be toggled."
  (interactive
   (list
    (let ((vals)
          (curr-sym
           (let ((result))
             (save-excursion
               (with-syntax-table emacs-lisp-mode-syntax-table
                 (while (progn
                          (setq result
                                (let ((sexp
                                       (sexp-at-point)))
                                  (pcase sexp
                                    (`(defcustom ,(and (pred (symbolp))
                                                   symb-name
                                                   (guard (not (memq symb-name '(t nil)))))
                                        ,(or 't 'nil)
                                        ,(pred (stringp))
                                        . ,_pl)
                                     symb-name))))
                          (and (not result)
                               (let ((parse-sexp-ignore-comments
                                      t)
                                     (pos (point)))
                                 (when-let ((str-start
                                             (nth 8
                                                  (syntax-ppss
                                                   (point)))))
                                   (goto-char str-start))
                                 (condition-case nil
                                     (progn
                                       (backward-up-list 1)
                                       t)
                                   (error (goto-char pos)
                                          nil))))))))
             result))
          (initial-input))
      (mapatoms
       (lambda (s)
         (if-let ((ctype
                   (and
                    (symbolp
                     s)
                    (ignore-errors
                      (get s 'custom-type)))))
             (pcase ctype
               ('boolean
                (when (eq curr-sym s)
                  (setq initial-input (symbol-name curr-sym)))
                (push
                 (list s
                       (get s 'variable-documentation)
                       (symbol-value s))
                 vals)))
           (when (and (symbolp s)
                      (boundp s)
                      (string-match-p (symbol-name s) "-debug$"))
             (let ((value (symbol-value s)))
               (when (or (eq value t)
                         (not value))
                 (push
                  (list s
                        (get s 'variable-documentation)
                        value)
                  vals)))))))
      (let* ((alist vals)
             (strs
              (seq-sort-by (lambda (str)
                             (cond ((and initial-input
                                         (string= initial-input str))
                                    -1)
                                   ((member str
                                            tray-builder--boolean-variable-history)
                                    0)
                                   (t (string-to-char str))))
                           #'<
                           (mapcar
                            (pcase-lambda
                              (`(,k .
                                 ,_))
                              (substring-no-properties
                               (symbol-name
                                k)))
                            vals)))
             (len (apply #'max (mapcar (pcase-lambda (`(,k . ,_))
                                         (length (symbol-name k)))
                                       vals)))
             (annotf (lambda (str)
                       (pcase-let
                           ((`(,doc ,value)
                             (cdr (assq (intern str) alist))))
                         (concat
                          (propertize " " 'display `(space :align-to
                                                     ,(1+ len)))
                          (if value "[X]" "[ ]")
                          " "
                          (truncate-string-to-width
                           (string-join
                            (split-string
                             (if
                                 (stringp
                                  doc)
                                 doc
                               "")
                             "[\t\n\r\f]")
                            " ")
                           (- (window-width)
                              (+ 10 len))))))))
        (intern
         (completing-read "Toggle: "
                          (lambda (str pred action)
                            (if (eq action 'metadata)
                                `(metadata
                                  (annotation-function .
                                   ,annotf))
                              (complete-with-action action
                                                    strs str
                                                    pred)))
                          nil t
                          nil
                          'tray-builder--boolean-variable-history))))))
  (when (get sym 'custom-type)
    (let ((val (symbol-value sym)))
      (funcall (or (get sym 'custom-set) 'set-default) sym (not val)))))




(defun tray-builder--make-toggle-suffix (cmd &optional variable description
                                             align)
  "Create a toggle suffix for a command.

Argument CMD is the command to be toggled.

Optional argument VARIABLE is the variable to be toggled.

Optional argument DESCRIPTION is the description of the toggle.

Optional argument ALIGN is the column to align the toggle's description."
  (list cmd :description (tray-builder--make-toggled-description
                          variable
                          description
                          align)))






(defun tray-builder--get-filtered-toggle-suffixes ()
  "Filter command suffixes based on requirements."
  (seq-filter
   (pcase-lambda (`(,cmd . ,v))
     (and (or (not (plist-get v :if-require))
              (not (seq-find (lambda (it)
                               (not
                                (require it nil t)))
                             (plist-get v :if-require))))
          (commandp cmd)))
   tray-builder-toggle-suffixes))


(defun tray-builder--generate-toggle-suffixes ()
  "Generate a list of key-command-description mappings."
  (let ((suffixes (tray-builder--get-filtered-toggle-suffixes))
        (cmds)
        (used-keys)
        (generated)
        (len))
    (setq len (tray-builder--first-column-children-len suffixes))
    (pcase-dolist (`(,cmd . ,pl)
                   suffixes)
      (if-let ((key (plist-get pl :key)))
          (push key used-keys)
        (push (cons cmd pl) cmds)))
    (setq generated (tray-builder-generate-shortcuts cmds (lambda (it)
                                                            (symbol-name (car it)))
                                                     (lambda (key cell)
                                                       (pcase-let*
                                                           ((`(,cmd . ,pl)
                                                             cell))
                                                         (append (list
                                                                  cmd
                                                                  :key
                                                                  key)
                                                                 pl)))
                                                     used-keys))
    (seq-map-indexed
     (lambda (item i)
       (pcase-let ((`(,cmd . ,pl) item))
         (let* ((key (or (plist-get pl :key)
                         (plist-get (cdr (assq cmd generated)) :key)))
                (descr (plist-get pl :description))
                (tranprops (tray-builder--plist-pick
                            (mapcar 'car
                                    tray-builder-transient-options)
                            pl)))
           (append (list key cmd
                         :description (if (functionp descr)
                                          descr
                                        (tray-builder--make-toggled-description
                                         (or (plist-get pl :variable-indicator)
                                             cmd)
                                         descr
                                         (if (>= i len)
                                             (* 2 tray-builder-align-toggle-num)
                                           tray-builder-align-toggle-num))))
                   tranprops))))
     suffixes)))

(defvar tray-builder--mapped-suffixes nil)

(defun tray-builder--first-column-children-len (items)
  "Calculate half the length of ITEMS, rounded to nearest integer.

Argument ITEMS is a list whose length is divided by 2 and rounded to the nearest
integer."
  (round (/
          (float
           (length
            items))
          2)))

;;;###autoload (autoload 'tray-builder-toggle-menu "tray-builder" nil t)
(transient-define-prefix tray-builder-toggle-menu ()
  "Toggle menu options with dynamic entries from `tray-builder-toggle-suffixes'."
  :refresh-suffixes t
  :transient-suffix t
  [[:description
    "Toggle"
    :setup-children
    (lambda (_args)
      (let ((group (seq-take tray-builder--mapped-suffixes
                             (tray-builder--first-column-children-len
                              tray-builder--mapped-suffixes))))
        (transient-parse-suffixes
         transient--prefix
         (apply #'vector
                group))))]
   [:description ""
    :setup-children
    (lambda (_args)
      (let ((group (seq-drop tray-builder--mapped-suffixes
                             (tray-builder--first-column-children-len
                              tray-builder--mapped-suffixes))))
        (transient-parse-suffixes
         transient--prefix
         (apply #'vector
                group))))]
   ["Other"
    ("." tray-builder-eval-toggle-minor-mode-prefix
     :description
     "Show all modes"
     :transient nil)
    ("!" "Toggle variable" tray-builder-toggle-custom-boolean-var :transient nil)]]
  (interactive)
  (setq tray-builder--mapped-suffixes (tray-builder--generate-toggle-suffixes))
  (transient-setup 'tray-builder-toggle-menu))

(provide 'tray-builder)
;;; tray-builder.el ends here