;; Enable line number
(global-linum-mode 1)
(column-number-mode 1)

;; Setup ido-mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Newline and indent
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq next-line-add-newlines t)
(setq js-indent-level 2)

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil) ; emacs 23.1, 24.2, default to t

;; set default tab char's display width to 4 spaces
(setq-default
  tab-width 2                                ; Set tab stops
  tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22
    24 26 28 30 32 34 36 38 40 42)
  )

;; make tab key always call a indent command.
(setq tab-always-indent nil)

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(font-use-system-font t)
 '(ruby-deep-indent-paren nil))
(put 'set-goal-column 'disabled nil)

;; Setup external libraries
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/color-theme-6.6.0/")
(require 'cl-lib)


;; Custom functions
(defun shell-other-window ()
  "Open a 'shell' in a new window."
  (interactive)
  (let ((buf(shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-frame buf)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Setup markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "gfm-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Setup for ruby
(setq ruby-insert-encoding-magic-comment nil)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

;; Enable clipboard
(setq x-select-enable-clipboard t)

;; Unique buffer name style
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Setup color theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-billw)
(and
 (eq window-system 'x)
 (color-theme-billw))

;; Setup org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done 'time)
(global-font-lock-mode 1)
(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red")
               ))

(global-visual-line-mode 1)
