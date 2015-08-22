;; please don't forget to install following packages
;; auto-complete popup smartscan use-package

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

(setq next-line-add-newlines t)

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(require 'use-package)

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
