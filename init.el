;; please don't forget to install following packages
;; auto-complete popup smartscan use-package elpy js2-mode magit
;; markdown-mode multiple-cursors paredit projectile typo yaml-mode window-purpose
;; site-lisp
;; https://raw.githubusercontent.com/purcell/elisp-slime-nav/master/elisp-slime-nav.el
;; https://raw.githubusercontent.com/jorgenschaefer/comint-scroll-to-bottom/master/comint-scroll-to-bottom.el
;; https://raw.githubusercontent.com/jorgenschaefer/emacs-ixio/master/ixio.el
;; https://raw.githubusercontent.com/jorgenschaefer/legalese/master/legalese.el
;; https://raw.githubusercontent.com/jorgenschaefer/pyvenv/master/pyvenv.el
;; python packages for elpy: jedi flake8 importmagic autopep8, clone
;; this repository https://github.com/sellout/emacs-color-theme-solarized.git

;; Bugfix until #20356 is fixed.
(set-terminal-parameter nil 'xterm--set-selection nil)

;; I dislike this fancy stuff. It's not always defined, though.
(dolist (mode '(tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode
                              menu-bar-mode blink-cursor-mode))
  (when (fboundp mode)
    (funcall mode -1)))
(when (window-system)
  (set-frame-font
   "-bitstream-bitstream vera sans mono-*-r-*-*-17-*-*-*-*-*-*-*")
  (setq x-select-enable-primary t
        x-select-enable-clipboard nil
        x-stretch-cursor t
        mouse-yank-at-point t))
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "utf-8")
(prefer-coding-system 'latin-1)
(prefer-coding-system 'utf-8)

(mapc (lambda (map)
        (define-key input-decode-map
          (read-kbd-macro (cadr map))
          (read-kbd-macro (car map))))
      '(("<backtab>"    "ESC [ Z")

;        ("<S-up>"       "ESC [1;2A")
;        ("<S-down>"     "ESC [1;2B")
;        ("<S-right>"    "ESC [1;2C")
;        ("<S-left>"     "ESC [1;2D")

;        ("<M-up>"       "ESC [1;3A")
;        ("<M-down>"     "ESC [1;3B")
;        ("<M-right>"    "ESC [1;3C")
;        ("<M-left>"     "ESC [1;3D")

;        ("<M-S-up>"     "ESC [1;4A")
;        ("<M-S-down>"   "ESC [1;4B")
;        ("<M-S-right>"  "ESC [1;4C")
;        ("<M-S-left>"   "ESC [1;4D")

;        ("<C-up>"       "ESC [1;5A")
;        ("<C-down>"     "ESC [1;5B")
;        ("<C-right>"    "ESC [1;5C")
;        ("<C-left>"     "ESC [1;5D")

;        ("<C-prior>"    "ESC [5;5~")
;        ("<C-next>"     "ESC [6;5~")
;        ("<C-delete>"   "ESC [3;5~")
;         ("M", "")
        ))

(column-number-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Put backup files neatly away
(let ((backup-dir "~/.cache/tmp/emacs/backups")
      (auto-saves-dir "~/.cache/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
	auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
	auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
	tramp-backup-directory-alist `((".*" . ,backup-dir))
	tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too
;; Random default values
(setq-default
 major-mode 'text-mode
;; initial-buffer-choice 'remember-notes
 scroll-preserve-screen-position 'keep
 user-mail-address (when (file-exists-p "~/.email")
		     (with-temp-buffer
		       (insert-file-contents "~/.email")
		       (goto-char (point-min))
		       (buffer-substring-no-properties
			(point) (point-at-eol))))
 inhibit-startup-message t
 cursor-in-non-selected-windows nil
 kill-whole-line t
 switch-to-buffer-preserve-window-point t
  load-prefer-newer t)

;; Case insensitivity
(setq case-fold-search t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Clean up whitespace
(setq-default indent-tabs-mode nil
	      delete-trailing-lines t
	      require-final-newline t
	      show-trailing-whitespace nil)
(add-hook 'prog-mode-hook (lambda ()
			    (setq-local show-trailing-whitespace t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom commands
(global-set-key (kbd "M-SPC") 'fc/delete-space)
(defun fc/delete-space ()
    "Remove all space around point.
Calling this repeatedly will clean more and more whitespace.
First, it will clear all whitespace until the end of the line, if
any. Then it will clear whitespace to the beginning of the line.
Then it will clear all following whitespace over any number of
lines. And then it will clear all preceding whitespace."
    (interactive)
    (cond
     ((looking-at "[ \t]+")
      (replace-match ""))
     ((looking-back "[ \t]")
      (let ((start (point)))
	(skip-chars-backward " \t")
	(delete-region (point) start)))
     ((looking-at "[ \t\n]+")
      (replace-match ""))
     ((looking-back "[ \t\n]")
      (let ((start (point)))
	(skip-chars-backward " \t\n")
	(delete-region (point) start)))))

(global-set-key [remap move-beginning-of-line] 'fc/move-beginning-of-line)
(defun fc/move-beginning-of-line ()
  "Move to indentation, or beginning of the line."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1))
  ;; (let ((current (point)))
  ;;   (back-to-indentation)
  ;;   (when (= (point) current)
  ;;     (move-beginning-of-line 1)))
  )
(global-set-key (kbd "C-x r a") 'fc/add-rectangle)
(defun fc/add-rectangle (start end)
    "Add all the lines in the region-rectangle and put the result in the
kill ring."
    (interactive "r")
    (let ((sum 0))
      (mapc (lambda (line)
	      (string-match "-?[0-9.]+" line)
	      (setq sum (+ sum (string-to-number (match-string 0 line)))))
	    (extract-rectangle start end))
      (kill-new (number-to-string sum))
      (message "%s" sum)))

(global-set-key (kbd "C-c e") 'fc/eval-and-replace)
(defun fc/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
	 (current-buffer)))

(global-set-key (kbd "C-c m") 'fc/calculate-region)
(defun fc/calculate-region (start end &optional prefix)
    "Evaluate the mathematical expression within the region, and
replace it with its result.
With a prefix arg, do not replace the region, but instead put the
result into the kill ring."
    (interactive "r\nP")
    (let* ((expr (buffer-substring start end))
	   (result (fc/bc-calculate-expression expr))
	   (ends-with-newline (string-match "\n$" expr)))
      (if prefix
	  (progn
	    (kill-new result)
	    (message "%s" result))
	(kill-region start end)
	(insert result)
	(when ends-with-newline
	  (insert "\n")))))

(defun fc/bc-calculate-expression (expr)
    "Evaluate `expr' as a mathematical expression, and return its result.
This actually pipes `expr' through bc(1), replacing newlines with
spaces first. If bc(1) encounters an error, an error is
signalled."
    (with-temp-buffer
      (insert expr)
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
	(replace-match " " nil t))
      (goto-char (point-max))
      (insert "\n")
      (call-process-region (point-min)
			   (point-max)
			   "bc" t t nil "-lq")
      (goto-char (point-min))
      (when (search-forward "error" nil t)
	(error "Bad expression"))
      (while (search-forward "\n" nil t)
	(replace-match "" nil t))
      (buffer-string)))

(global-set-key (kbd "C-c d") 'fc/insert-date)
(defun fc/insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
		   ((not prefix) "%Y-%m-%d")
		   ((equal prefix '(4)) "%d.%m.%Y")
		   (t "%A, %d. %B %Y")))
	  (system-time-locale "de_DE"))
      (insert (format-time-string format))))

(global-set-key (kbd "C-c C-u") 'fc/kill-to-beginning-of-line)
(defun fc/kill-to-beginning-of-line ()
  "Kill from the beginning of the line to point."
  (interactive)
  (kill-region (point-at-bol)
	       (point)))
(global-set-key (kbd "C-x 8 p") 'fc/unicode-info-at-point)
(defun fc/unicode-info-at-point (&optional do-kill)
  "Display the unicode name of the character at point."
  (interactive "P")
  (let ((char-code (elt (thing-at-point 'char) 0))
	name)
    (setq name (get-char-code-property char-code 'name))
    (when (or (not name)
	      (= ?< (elt name 0)))
      (setq name (get-char-code-property char-code 'old-name)))
    (when do-kill
      (kill-new name))
    (message "%s" name)))
(defun rename-buffer-and-file (new-file-name)
    "Rename the current buffer's file to NEW-FILE-NAME.

Also, rename the buffer and attach it to the new file."
    (interactive
     (list (read-file-name "Rename to: "
			   nil buffer-file-name
			   nil buffer-file-name)))
    (rename-file buffer-file-name
		 new-file-name)
    (rename-buffer (file-name-nondirectory new-file-name)
		   t)
    (setq buffer-file-name new-file-name)
    (normal-mode))
(global-set-key (kbd "C-c u") 'unfill-paragraph)
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, "
	  "sed do eiusmod tempor incididunt ut labore et dolore "
	  "magnaaliqua. Ut enim ad minim veniam, quis nostrud "
	  "exercitation ullamco laboris nisi ut aliquip ex ea commodo "
	  "consequat. Duis aute irure dolor in reprehenderit in "
	  "voluptate velit esse cillum dolore eu fugiat nulla pariatur. "
	  "Excepteur sint occaecat cupidatat non proident, sunt in "
	  "culpa qui officia deserunt mollit anim id est laborum."))
(defun google (what)
  "Use google to search for WHAT."
  (interactive "sSearch: ")
  (browse-url (format "http://www.google.de/search?q=%s" what)))

(defun leo (word)
  (interactive "sWord: ")
  (browse-url (format "http://dict.leo.org/?search=%s" word)))
(global-set-key (kbd "C-c l") 'leo-at-point)
(defun leo-at-point ()
  "Open the Leo dictionary for the word at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if (not word)
	(error "No word found at point")
      (browse-url (format "http://dict.leo.org/?search=%s#results"
			  word)))))

(defun fc/htmlfontify-buffer-or-region (for-blog-p)
    "Show the current buffer or region if active as HTML in a temporary buffer.
This uses `htmlfontify'."
    (interactive "P")
    (let ((hfy-page-footer (lambda (filename)
			     ""))
	  (hfy-page-header (lambda (filename stylesheet)
			     (if for-blog-p
				 "<link href=\"http://www.jorgenschaefer.de/css/elisp.css\" rel=\"stylesheet\" type=\"text/css\">\n"
			       ""))))
      (if (region-active-p)
	  (let ((text (buffer-substring (region-beginning)
					(region-end))))
	    (with-temp-buffer
	      (insert text)
	      (switch-to-buffer (htmlfontify-buffer))))
	(switch-to-buffer (htmlfontify-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various global modes shipped with Emacs

;;;;;;;;;;;;;
;; compile.el

(global-set-key (kbd "<f5>") 'recompile)
;;;;;;;;;;;;
;; delsel.el
(load "delsel" nil t)
(delete-selection-mode 1)
;;;;;;;;;;
;; diff.el
(setq diff-switches "-u")

;;;;;;;;;;;;;;;
;; elec-pair.el
(when (load "elec-pair" t t)
  (electric-pair-mode 1)
  (defun fc/electric-pair-inhibit (char)
        "Return t if we want to not pair this char.
Don't pair the closing paren in :-("
	(or (and (eq char ?\()
		 (looking-back ":-("))
	    (electric-pair-default-inhibit char)))
  (setq electric-pair-inhibit-predicate 'fc/electric-pair-inhibit)
  (global-set-key (kbd "M-\"") 'fc/electric-pair-meta-quote)
  (defun fc/electric-pair-meta-quote ()
    "Wrap quotes around the following symbol."
    (interactive)
    (insert "\"")
    (save-excursion
      (forward-sexp 1)
      (insert "\"")))
  (global-set-key (kbd "M-(") 'fc/electric-pair-meta-paren)
  (defun fc/electric-pair-meta-paren ()
    "Wrap parens around the following symbol."
    (interactive)
    (insert "(")
    (save-excursion
      (forward-sexp 1)
      (insert ")"))))

;;;;;;;;;;;;;;
;; electric.el

(load "electric" nil t)
(electric-indent-mode 1)

;;;;;;;;;
;; eww.el

(when (not (getenv "DISPLAY"))
  (when (load "eww" nil t)
    (setq browse-url-browser-function 'eww-browse-url)))

;;;;;;;;;;
;; ffap.el
(global-set-key (kbd "C-c f") 'find-file-at-point)

;;;;;;;;;;;;;;;;
;; hippie-exp.el

(load "hippie-exp" nil t)
(global-set-key (kbd "M-/") 'hippie-expand)
;; Full-line completion is *annoying*
(setq hippie-expand-try-functions-list
      (delq 'try-expand-list
	    (delq 'try-expand-line
		  hippie-expand-try-functions-list)))

;;;;;;;;;;;;;
;; ibuffer.el
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-hook 'fc/ibuffer-group-buffers)
(defun fc/ibuffer-group-buffers ()
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-filter-groups
	(append
;;	 (ibuffer-vc-generate-filter-groups-by-vc-root)
	 '(("Circe"
	    (or (mode . circe-channel-mode)
		(mode . circe-query-mode)
		(mode . circe-server-mode))))
;;	 (ibuffer-projectile-generate-filter-groups)
         ))
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

;;;;;;;;;
;; ido.el

(load "ido" nil t)
(ido-mode 1)
(setq ido-everywhere t
      ido-confirm-unique-completion t
      ;; This is cute. Except when you want to open a new file, then
      ;; it's annoying as hell.
      ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t)
(add-hook 'ido-setup-hook 'fc/ido-setup)
(defun fc/ido-setup ()
  (define-key ido-common-completion-map (kbd "C-c")
    (make-sparse-keymap))
  (define-key ido-common-completion-map (kbd "C-c C-u")
    'fc/ido-copy-selection)
  (define-key ido-file-dir-completion-map (kbd "<up>")
    'ido-prev-work-directory)
  (define-key ido-file-dir-completion-map (kbd "<down>")
    'ido-next-work-directory))

(defun fc/ido-copy-selection ()
  "Copy the current ido selection to the kill ring."
  (interactive)
  (kill-new (abbreviate-file-name (concat ido-current-directory
					  ido-text))))


;;;;;;;;;;;;;;
;;; isearch.el

(define-key isearch-mode-map (kbd "C-d") 'fc/isearch-yank-symbol)
(defun fc/isearch-yank-symbol ()
    "Yank the symbol at point into the isearch minibuffer.
C-w does something similar in isearch, but it only looks for the
rest of the word. I want to look for the whole string. And
symbol, not word, as I need this for programming the most."
    (interactive)
    (isearch-yank-string
     (save-excursion
       (when (and (not isearch-forward)
		  isearch-other-end)
	 (goto-char isearch-other-end))
       (thing-at-point 'symbol))))

;;;;;;;;;;;;;;;;
;; newcomment.el

(setq comment-style 'extra-line
      comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook 'auto-fill-mode)


;;;;;;;;;;;
;; paren.el
(load "paren" nil t)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;;;;;;;;;;;;;;;
;; pcomplete.el

;; I prefer bash-style to zsh-style
(setq pcomplete-cycle-completions nil)

;;;;;;;;;;;;;;;;
;; paragraphs.el

(load "paragraphs" nil t)
;; Single dash starts a paragraph
(setq paragraph-start "- \\|\f\\|[ \t]*$"
      paragraph-separate "[\f\t ]*$"
      sentence-end-double-space nil)

;;;;;;;;;;;;;;;
;; saveplace.el

(load "saveplace" nil t)
(setq-default save-place t)

;;;;;;;;;;;;
;; server.el

(load "server" nil t)
(when (not (server-running-p server-name))
  (server-start))

;;;;;;;;;;;;;
;; subword.el

(load "subword" nil t)
(global-subword-mode)
(let ((elt (assq 'subword-mode minor-mode-alist)))
  (when elt
    (setcdr (assq 'subword-mode minor-mode-alist) '(""))))
;;;;;;;;;;
;; time.el
(load "time" nil t)
(setq display-time-24hr-format t)
(display-time)
;; Some nice times for M-x display-time-world
(setq display-time-world-list '(("America/New_York" "Sarasota")
				("Europe/Warsaw" "Warsaw")
				))


;;;;;;;;;;;;;;;;;
;;; time-stamp.el

(add-hook 'before-save-hook 'time-stamp)

;;;;;;;;;;;;
;;; tramp.el

(require 'cl-lib)
(setq file-name-handler-alist
      (cl-remove-if  (lambda (elt)
		       (string-match "tramp" (symbol-name (cdr elt))))
		     file-name-handler-alist))
;;;;;;;;;;;;;;;
;;; uniquify.el
(load "uniquify" nil t)
(setq-default uniquify-buffer-name-style 'post-forward)

;;;;;;;;
;; vc.el

(setq vc-diff-switches diff-switches)

;;;;;;;;;;;;;;
;; windmove.el

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;;;;;;;;;;;;
;; winner.el

(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various major modes shipped with Emacs

;;;;;;;;;;
;;; c-mode
(add-hook 'c-mode-hook 'fc/c-setup)
(defun fc/c-setup ()
  "Set up C mode for my needs."
  (c-set-style "k&r")
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (c-set-offset 'inextern-lang 0))
;;;;;;;;;;;;;;
;; comint-mode
(load "comint" nil t)
(add-hook 'comint-mode-hook 'fc/init-comint)
(defun fc/init-comint ()
  ;; Don't jump around when output in a buffer happens
  (set (make-local-variable 'scroll-conservatively) 1000))
(add-to-list 'comint-output-filter-functions
	     'comint-watch-for-password-prompt)
(ansi-color-for-comint-mode-on)
;;;;;;;;;;;;;;;;;;;
;; compilation-mode
(load "compile" nil t)
(load "ansi-color" nil t)
(defun fc/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'fc/colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;
;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(defun elisp-check ()
    "Check the current buffer for possible elisp problems.
This actually byte compiles the buffer, but throws away the
result and keeps only the warnings."
    (interactive)
    (let ((lisp (buffer-substring-no-properties (point-min)
						(point-max))))
      (with-temp-buffer
	(setq buffer-file-coding-system nil)
	(set-buffer-multibyte t)
	(insert lisp)
	(let ((byte-compile-log-buffer (format "*Check for %s*"
					       (buffer-name)))
	      (byte-compile-verbose nil))
	  (byte-compile-from-buffer (current-buffer))))))
;;;;;;;;;;;;;
;;; html-mode
(load "sgml-mode" nil t)
(define-key html-mode-map (kbd "C-c RET") 'fc/html-toggle-paragraph)
(defun fc/html-toggle-paragraph ()
  "Add or remove HTML paragraph tags from the current paragraph"
  (interactive)
  (save-excursion
    (backward-paragraph)
    (when (looking-at "^\\s-*$")
      (forward-char 1))
    (if (looking-at "<p>")
	(replace-match "")
      (insert "<p>"))
    (forward-paragraph)
    (when (looking-at "^\\s-*$")
      (backward-char 1))
    (if (looking-back "</p>")
	(replace-match "")
      (insert "</p>"))))

(define-key html-mode-map (kbd "&") 'fc/html-insert-quoted)
(defun fc/html-insert-quoted (char)
    "Insert a & character.
Depending on the subsequent character, insert an appropriate HTML
glyph."
    (interactive "cInsert: ")
    (case char
	  ((?&)
	   (insert "&amp;"))
	  ((?<)
	   (insert "&lt;"))
	  ((?>)
	   (insert "&gt;"))
	  ((?\s)
	   (insert "&nbsp;"))
	  ((34) ; "
	   (insert "&quot;"))
	  (t
	        (insert (format "&#x%02x;" char)))))

;;;;;;;;;;;;;;;;;;;
;;; javascript-mode
;; For json only, really. js2-mode doesn't do a good job with json.

(load "js")
(setq-default js-indent-level 2)

;;;;;;;;;;;;;;;
;;; python-mode

(load "python" nil t)

(when (executable-find "flake8")
  (setq python-check-command "flake8"))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq electric-indent-chars '(10))))

;;;;;;;;;;;;;;
;; scheme-mode

;; Make parens less visible
(font-lock-add-keywords 'scheme-mode '(("[()]" . 'paren-face)))
(defface paren-face
  '((t (:foreground "gray60")))
  "The face used for parenthesises."
  :group 'scheme)

;;;;;;;;;;;;;
;; shell-mode
(load "shell" nil t)
;; > may show up in some prompts
(setq shell-prompt-pattern "^[^#$%\n]*[#$%>] *")
(global-set-key (kbd "C-c s") 'fc/toggle-shell)
(defun fc/toggle-shell ()
  "Switch between the last active buffer and the shell."
  (interactive)
  (if (eq major-mode 'shell-mode)
      (let ((buf (catch 'return
		   (dolist (buf (cdr (buffer-list)))
		     (when (not (string-prefix-p " " (buffer-name buf)))
		       (throw 'return buf)))
		   nil)))
	(when buf
	  (switch-to-buffer buf)))
    (shell)))
(define-key shell-mode-map (kbd "C-c C-y") 'fc/shell-switch-dir)
(defun fc/shell-switch-dir ()
  "Switch `shell-mode' to the `default-directory' of the last buffer."
  (interactive)
  (when (eq major-mode 'shell-mode)
    (let* ((dir (catch 'return
		  (dolist (buf (buffer-list))
		    (with-current-buffer buf
		      (when buffer-file-name
			(throw 'return default-directory)))))))
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (insert (format "cd %s" (shell-quote-argument dir)))
      (let ((comint-eol-on-send nil))
	(comint-send-input)))))

;;;;;;;;;;;;
;; text-mode
(add-hook 'text-mode-hook 'fc/text-mode-init)
(defun fc/text-mode-init ()
  (setq-local electric-pair-preserve-balance nil))

(setq next-line-add-newlines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Third party extensions
(when (file-directory-p "~/.emacs.d/site-lisp")
  (rename-file "~/.emacs.d/site-lisp" "~/.emacs.d/lisp"))
(when (file-directory-p "~/.emacs.d/lisp")
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (dolist (dirname (directory-files "~/.emacs.d/lisp" t "^[^.]"))
    (when (file-directory-p dirname)
      (add-to-list 'load-path dirname))))
(load "package" nil t)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("elpy" . "http://jorgenschaefer.github.io/packages/")))
(setq package-archive-priorities '(("melpa" . -100))
      package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comint-scroll-to-bottom

(when (load "comint-scroll-to-bottom" t t)
  (add-hook 'comint-mode-hook 'comint-add-scroll-to-bottom))

;;;;;;;;;;;;;;;;;;
;; elisp-slime-nav

(when (load "elisp-slime-nav" t t)
  (define-key elisp-slime-nav-mode-map (kbd "M-,") nil)
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode))

;;;;;;;
;; elpy

(when (load "elpy" t t)
  (elpy-enable)

  (global-set-key (kbd "C-c ,") 'elpy-multiedit)
  (add-hook 'pyvenv-post-activate-hooks 'fc/configure-elpy-from-env)
  (defun fc/configure-elpy-from-env ()
    (dolist (elt process-environment)
      (when (string-match "\\`\\(ELPY_[^=]*\\)=\\(.*\\)\\'" elt)
	(let ((var (downcase
		    (replace-regexp-in-string "_" "-" (match-string 1 elt))))
	      (val (match-string 2 elt)))
	  (set (intern var) (read val)))))))

;;  (elpy-use-ipython)

 ;;;;;;;
;; ixio
(load "ixio" t t)
;;;;;;;;;;;
;; js2-mode
(when (load "js2-mode" t t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'auto-complete-mode)
  (setq-default js2-highlight-level 3
		js2-mode-indent-ignore-first-tab t
		js2-mode-indent-inhibit-undo t
		js2-global-externs '("$")
		js2-basic-offset 2
		js2-global-externs
		'(;; AngularJS
		  "angular" "module" "inject"
		  ;; Jasmine
		  "describe" "it" "expect" "beforeEach" "spyOn")
		)
  (define-key js2-mode-map (kbd "C-c C-n") 'js2-next-error)

  (defadvice js2-mode-toggle-element (before ad-move-to-toggle-element
					     activate)
    "Move to a sensible location first"
    (interactive)
    (let* ((start (line-beginning-position))
	   (end (line-end-position))
	   (invisible (next-single-property-change start 'invisible nil end))
	   (brace (save-excursion
		    (goto-char start)
		    (re-search-forward "{" end t))))
      (cond
       (invisible (goto-char invisible))
       (brace (goto-char brace))))))

;;;;;;;;;;;
;; legalese
(load "legalese" t t)
;;;;;;;;
;; magit
(setq magit-last-seen-setup-instructions "1.4.0")
(when (load "magit" t t)
  (global-set-key (kbd "C-x v g") 'magit-status)
  (global-set-key (kbd "C-x v a") 'vc-annotate))

;;;;;;;;;;;;;;;;;
;;; markdown-mode
(when (load "markdown-mode" t t)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (setq markdown-command "markdown_py"
	markdown-italic-underscore t
	markdown-indent-on-enter nil
	markdown-follow-wiki-link-on-enter nil
	)
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
  (define-key markdown-mode-map (kbd "TAB") 'markdown-cycle)
  (define-key markdown-mode-map (kbd "<backtab>") 'markdown-shifttab)
  (define-key markdown-mode-map (kbd "C-M-f") 'forward-sexp)
  (define-key markdown-mode-map (kbd "C-M-b") 'backward-sexp)
  (define-key markdown-mode-map (kbd "C-c C-x") 'fc/markdown-code-block)
  (defun fc/markdown-code-block (beg end)
    "Wrap the current region into a code block."
    (interactive "r")
    (save-excursion
      (goto-char end)
      (when (not (bolp))
	(insert "\n"))
      (insert "```\n")
      (goto-char beg)
      (forward-line 0)
      (insert "```\n")))

  (define-key markdown-mode-map (kbd "C-c C-n") 'fc/markdown-next-header)
  (defun fc/markdown-next-header ()
    "Go to the next header in the file."
    (interactive)
    (let ((next-header (save-excursion
			 (forward-line 1)
			 (re-search-forward "^#" nil t))))
      (if (not next-header)
	  (error "No next header")
	(goto-char next-header)
	(goto-char (point-at-bol)))))
  (define-key markdown-mode-map (kbd "C-c C-p") 'fc/markdown-previous-header)
  (defun fc/markdown-previous-header ()
    "Go to the previous header in the file."
    (interactive)
    (let ((previous-header (save-excursion
			     (forward-line -1)
			     (re-search-backward "^#" nil t))))
      (if (not previous-header)
	  (error "No previous header")
	(goto-char previous-header)
	(goto-char (point-at-bol)))))

  (defun markdown-check-change-for-wiki-link (&rest ignored)
        "Do nothing.
The default markdown implementation exhibits a bug. You can
reproduce it using the following:
M-: (when (looking-at \"\") (replace-match \"abc\"))
This will insert \"abc\" at the current point, but move point
down one line. Removing `markdown-check-change-for-wiki-link'
from `after-change-functions' fixes that."
	nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Cursors Mode

(when (load "multiple-cursors" t t)
  (global-set-key (kbd "<M-S-up>") 'mc/mark-previous-lines)
  (global-set-key (kbd "<M-S-down>") 'mc/mark-next-lines)
  (global-set-key (kbd "C-M-j") 'fc/mark-next-like-this)
  (global-set-key (kbd "<M-S-return>") 'fc/mark-next-like-this)
  (defun fc/mark-next-like-this (arg)
    (interactive "p")
    (when (not (use-region-p))
      (let* ((end (progn
		    (forward-sexp)
		    (point)))
	     (beg (progn
		    (backward-sexp)
		    (point))))
	(goto-char end)
	(set-mark beg)))
    (mc/mark-next-like-this arg)))

;;;;;;;;;;;
;; Org Mode

;; This actually comes with Emacs, but we want to use the one from GNU
;; ELPA as it is more current, hence it's down here.
(when (load "org" t t)
  (modify-syntax-entry ?\' "." org-mode-syntax-table)
  (define-key org-mode-map (kbd "C-c a") 'fc/org-agenda)
  (define-key org-mode-map (kbd "C-c ,") nil)
  (defun fc/org-agenda ()
    (interactive)
    (when (get-buffer "google-calendar.org")
      (kill-buffer "google-calendar.org"))
    (org-agenda nil (caar org-agenda-custom-commands)))

  (setq org-fontify-emphasized-text nil
	org-tags-column -76
	org-agenda-files nil
	org-descriptive-links nil
	org-agenda-include-diary nil
	org-agenda-start-on-weekday nil
	org-todo-keywords '((sequence "TODO"
				      "DONE"
				      "WAITING"
				      "SCHEDULED"
				      "ONGOING"))
	org-agenda-custom-commands '(("t" "General TODO agenda"
				      ((todo "TODO")
				       (agenda "")
				       (todo "WAITING")))))
  (dolist (filename '("~/Documents/Notes/Todo"
		      "~/Files/google-calendar.org"))
    (when (file-exists-p filename)
      (add-to-list 'org-agenda-files filename t))))

;;;;;;;;;;
;; paredit

(when (load "paredit" t t)
  (define-key paredit-mode-map (kbd "RET") 'newline)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)
  (define-key paredit-mode-map (kbd "<C-left>") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "<C-right>") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-q") 'paredit-reindent-defun)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode))

;;;;;;;;;;;;;
;; projectile
(when (load "projectile" t t)
  (projectile-global-mode))

;;;;;;;;;
;; pyvenv
(when (load "pyvenv" t t)
  (defalias 'workon 'pyvenv-workon)
  (pyvenv-mode))
;;;;;;;;;;
;; typo.el
(when (load "typo" t t)
  (dolist (hook '(markdown-mode-hook html-mode-hook))
    (add-hook hook 'typo-mode)))

;;;;;;;;;;;;
;; yaml-mode
(when (load "yaml-mode" t t)
  (define-key yaml-mode-map (kbd "C-j") nil)
  (add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode)))



;; auto-complete config
(ac-config-default)

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
		       (if (not (minibufferp (current-buffer)))
			   (auto-complete-mode 1))
		       ))
(real-global-auto-complete-mode t)
(setq ac-auto-start 3)

;; adjusting copy buffers, want to work with os buffer instead of emacs buffer
(xterm-mouse-mode 0)
(defun copy-to-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
	(message "Yanked region to x-clipboard!")
	(call-interactively 'clipboard-kill-ring-save)
	)
    (if (region-active-p)
	(progn
	  (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
	  (message "Yanked region to clipboard!")
	  (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
	(clipboard-yank)
	(message "graphics active")
	)
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )

(global-set-key [f8] 'copy-to-clipboard)
(global-set-key [f9] 'paste-from-clipboard)

;; smart scan stuff
(require 'use-package)
(use-package smartscan
  :init (global-smartscan-mode t)
)

;; dired setup
(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "c") 'my-dired-create-file)
     (defun create-new-file (file-list)
       (defun exsitp-untitled-x (file-list cnt)
         (while (and (car file-list) (not (string= (car file-list) (concat "untitled" (number-to-string cnt) ".txt"))))
           (setq file-list (cdr file-list)))
         (car file-list))

       (defun exsitp-untitled (file-list)
         (while (and (car file-list) (not (string= (car file-list) "untitled.txt")))
           (setq file-list (cdr file-list)))
         (car file-list))

       (if (not (exsitp-untitled file-list))
           "untitled.txt"
         (let ((cnt 2))
           (while (exsitp-untitled-x file-list cnt)
             (setq cnt (1+ cnt)))
           (concat "untitled" (number-to-string cnt) ".txt")
           )
         )
       )
     (defun my-dired-create-file (file)
       (interactive
        (list (read-file-name "Create file: " (concat (dired-current-directory) (create-new-file (directory-files (dired-current-directory))))))
        )
       (write-region "" nil (expand-file-name file) t) 
       (dired-add-file file)
       (revert-buffer)
       (dired-goto-file (expand-file-name file))
       )
     )
  )

(load "dired" nil t)
(define-key dired-mode-map (kbd "a") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "RET") 'dired-find-file)
(add-hook 'dired-mode-hook (lambda ()  (dired-hide-details-mode 1)))
(setq dired-omit-mode t)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files "^\\.$\\|\\.pdf$\\|\\.pyc$\\|\\.tex$\\|\\.egg-info$\\|^__pycache__$")

;; window-purpose
;;(require 'window-purpose)
(purpose-mode)
;;(add-to-list 'purpose-user-mode-purposes '(python-mode . py))
;;(add-to-list 'purpose-user-mode-purposes '(dired-mode . projectstructure))
;;(add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . misc))

;;(add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
;;(add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
;;(setq purpose-use-default-configuration t)
                                        ; not really necessary,
                                        ; default is t
;;(add-hook 'purpose-select-buffer-hook (lambda () (python-shell-switch-to-shell) ))
;;(purpose-compile-user-configuration) ; activates your changes
(global-set-key (kbd "M-L") 'purpose-x-code1-setup)
(global-set-key (kbd "<f4>") 'delete-window)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start environment
(when (file-exists-p "~/Documents/Notes/Todo.org")
  (with-current-buffer (find-file "~/Documents/Notes/Todo.org")
    (setq default-directory "~/"))
  (org-agenda nil "t")
  (other-window 1))
;; Faces
(face-spec-set 'font-lock-comment-face
	       '((((class color)) (:foreground "red"))))
(face-spec-set 'highlight-indent-face
	       '((((min-colors 64)) (:background "#f5f5f5"))
		 (t (:background "white"))))
(face-spec-set 'flymake-errline
	       '((((min-colors 64)) (:background "#ffafaf" :inherit nil))
		 (t (:foreground "red" :bold t))))
(face-spec-set 'org-tag '((t nil)))
(face-spec-set 'rst-level-1 '((t (:foreground "cyan" :background nil))))
(face-spec-set 'rst-level-2 '((t (:foreground "cyan" :background nil))))
(face-spec-set 'rst-level-3 '((t (:foreground "cyan" :background nil))))
(face-spec-set 'rst-level-4 '((t (:foreground "cyan" :background nil))))

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; revive and windowsmode
;;(autoload 'save-current-configuration "revive" "Save status" t)
;; (autoload 'resume "revive" "Resume Emacs" t)
;; (autoload 'wipe "revive" "Wipe Emacs" t)
;; (define-key ctl-x-map "S" 'save-current-configuration)
;; (define-key ctl-x-map "F" 'resume)
;; (define-key ctl-x-map "K" 'wipe)

;;(require 'session)

;;(add-hook 'after-init-hook 'session-initialize)


;;(setq desktop-restore-frames nil)

;; face customization
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(setq frame-background-mode 'dark)
(load-theme 'solarized t)
