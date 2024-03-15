;; CURRENTLY TESTING NEW CONFIG FILE

;;; Package Manager - Elpaca
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(elpaca-wait)


;;; General Emacs Settings

;; UI 
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(column-number-mode 1)
(global-visual-line-mode 1)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Editing
(delete-selection-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)

;; QOL
(setq make-backup-files nil)
(setq auto-save-default nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;; Packages

;; UI
(use-package ef-themes
  :ensure t
  :init
  (load-theme 'ef-frost t))

(use-package diminish
  :ensure t)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15))

;; Vertical Completion + Complementary Packages
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq enable-recursive-minibuffers t)
  (setq set-vertico-count 20)
  (setq vertico-cycle t))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-categor-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t)

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package emacs
  :ensure nil
  :init
  (setq completion-cycle-threshold 3)
  (savehist-mode 1))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Projects Management
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(use-package perspective
  :ensure t
  :init
  (persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p")))

;; Help
;; Another package to look at is helpful.el
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Development
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((prog-mode . lsp-prog-mode-hook)
  (lsp-mode . lsp-enable-which-key-integration))
  :commands
  (lsp lsp-deferred))

; Custom function to not turn on lsp-mode for emac
(defun lsp-prog-mode-hook ()
  "Custom hook for running LSP modes."
  (unless (eq major-mode 'emacs-lisp-mode)
    (lsp)))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-mode))

(use-package lsp-treemacs
  :ensure t
  :commands
  (lsp-treemacs-errors-list))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :commands dap-debug
  :config
  (setq dap-python-debugger 'debugpy)
  (require 'dap-python)
  (require 'dap-lldb))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package company
  :ensure t
  :after lsp-mode
  :custom
  (company-idle-delay .1)
  (company-minimum-prefix-length 1)
  (global-company-mode t))

(use-package magit
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package pet
  :ensure t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package markdown-mode
  :ensure t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "pandoc"))

;; Treesitter
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Language Settings
;; (setq c-ts-mode-indent-offset 4)

;; Terminal
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000))

(use-package multi-vterm
  :ensure t)

;; Org Mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (python . t)
   (java . t)
   (lisp . t)
   (js . t)
   (sql . t)))

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-bullets
  :ensure t)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Keybindings
(use-package general
  :ensure t
  :config
  (general-define-key

   ;; built-in commands
   "C-x C-b" 'persp-ibuffer
   "C-c f" 'recentf
   "M-o" 'other-window

   ;; consult
   "C-x r b" 'consult-bookmark

   ;; terminal
   "C-c n" 'multi-vterm

   ;; perspective
   "C-c p b" 'persp-ibuffer
   "C-c p s" 'persp-switch
   ))

;; Environment
(use-package envrc
  :ensure t
  :init
  (envrc-global-mode))
