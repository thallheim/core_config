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
;(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(setq show-paren-delay 0)
(setq show-paren-mode 1)

;; HELM
(use-package helm)
(global-set-key (kbd "C-c h t") 'helm-cmd-t)
; (global-set-key (kbd "C-c h g g") 'helm-git-grep)
; (global-set-key (kbd "C-c h g l") 'helm-ls-git-ls)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h a") 'helm-org-agenda-files-headings)
(global-set-key (kbd "C-c h r") 'helm-recentf)

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
  

;; MODES
 ; js2
(require 'js2-mode)
 (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
 (require 'js2-refactor)
 (require 'xref-js2)

 (add-hook 'js2-mode-hook #'js2-refactor-mode)
 (js2r-add-keybindings-with-prefix "C-c C-r")
 (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
 
 ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
 ;; unbind it.
 (define-key js-mode-map (kbd "M-.") nil)
 
 (add-hook 'js2-mode-hook (lambda ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
; Better imenu
 (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)



;; Yasnippet
  (add-to-list 'load-path
                "~/.emacs.d/plugins/yasnippet")
  (require 'yasnippet)
  (yas-global-mode 1)
  
  
(add-hook 'text-mode-hook 'visual-line-mode)            ;; Sensible line breaking
(delete-selection-mode t)                             ;; Overwrite selected text 
(setq scroll-error-top-bottom t)         ;; Scroll to the first and last line of the buffer


(require 'ido)      ;; Enable IDO  
(ido-mode t)
;; Display ido results vertically
;;(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

;; Smex
  

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
  ; prettier front-end for company
  (use-package company-box
    :hook (company-mode . company-box-mode))
  (global-set-key (kbd "C-c C-l") 'company-complete)


;; LSP-MODE
  (use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (less-css-mode . lsp)
	 (js2-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
  
  ;; extensions
  (use-package lsp-ui :commands lsp-ui-mode)
  ;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
  ;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
  ;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
  
  ;; Debugger
  ;;(use-package dap-mode)
  ;; (use-package dap-LANGUAGE) to load dap adapter for LANGUAGE
  
  ;; Which-key integration
  (use-package which-key
      :config
      (which-key-mode))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck :ensure)
