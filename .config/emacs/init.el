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

;; Font Setting
(set-face-attribute 'default nil :font "JetBrains Mono" :height 140)
(setq line-spacing 3)

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

(use-package mood-line
  :ensure t
  :init
  (mood-line-mode))

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

(use-package json-mode
  :ensure t
  :config
  (setq js-indent-level 2))

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

(with-eval-after-load 'org
  (require 'org-tempo))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (company-mode 0)))

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("baf6946d46390fd34b5f92b778be544c7dc1286bafb81c314dc7ee8e3f8875d3" "f47dac3dabaa1774bb96d93ccc59794541639fd342aedca4ce2b47ee7c55540f" "908727ee710b5af6a997520ca544e20643638713fd443dc5c6f74fbe6f0d4909" "f7dcdb56bc0c04b6abaecc335eacf3b5cf262396168a2e65791b0c6a3317c849" "2974955a855645c114cf862152e7e98f56e99c2f1f167f1a79a75d5703aa5b4c" "caf354a83744b946d1159c05b567a3812ef264076281b0b8808412cd0a50342a" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
