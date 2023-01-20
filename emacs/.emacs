;; Define the init file
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

;; Add MELPA blob to package list, then check if already installed and/or downlaod
;; Use M-x list-packages to refresh package list 
(require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; use-package to simplify the config file
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure 't)

;; Some basic UI stuff
(setq inhibit-startup-message t)
  (tool-bar-mode -1)
  ; (menu-bar-mode -1)
  (scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; BUFFER-EXPOSE
;; https://github.com/clemera/buffer-expose
(buffer-expose-mode 1)

 (defvar buffer-expose-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<s-tab>") 'buffer-expose)
     (define-key map (kbd "<C-tab>") 'buffer-expose-no-stars)
     (define-key map (kbd "C-c <C-tab>") 'buffer-expose-current-mode)
     (define-key map (kbd "C-c C-m") 'buffer-expose-major-mode)
     (define-key map (kbd "C-c C-d") 'buffer-expose-dired-buffers)
     (define-key map (kbd "C-c C-*") 'buffer-expose-stars)
     map)
   "Mode map for command `buffer-expose-mode'.")


;; Theme
  (use-package exotica-theme
    ; :config (load-theme 'exotica t))
    :config (load-theme 'deeper-blue t))

;; JS-runetime... runner..
;;    (require 'js-comint)
;;    (setq `js-comint-program-command "/usr/bin/node")
;;    (add-hook 'js2-mode-hook
;;            (lambda ()
;;              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
;;              (local-set-key (kbd "C-c b") 'js-send-buffer)))


(add-hook 'text-mode-hook 'visual-line-mode)            ;; Sensible line breaking
  

  (delete-selection-mode t)                             ;; Overwrite selected text 
  

  (setq scroll-error-top-bottom t)         ;; Scroll to the first and last line of the buffer


(require 'ido)      ;; Enable IDO  
(ido-mode t)
;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

;; Smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; fuck yeah which-key!
(use-package which-key
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.5
          which-key-idle-secondary-delay 0.5)
    (which-key-setup-side-window-bottom))

;; Auto completion
  (use-package company
    :config
    (setq company-idle-delay 0
          company-minimum-prefix-length 1
          company-selection-wrap-around t))
  (global-company-mode)


