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

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.
;; (elpaca example-package)
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; (elpaca nil (message "deferred"))

(setq package-enable-at-startup nil)
(package-initialize)

(use-package general
  :config
  (general-define-key
   ;; remap list buffers to use ibuffer
   "C-x C-b" 'ibuffer
   ;; remap switching windows
   "M-o" 'other-window
   ;; add keybinding for opening config.org file
   "C-c c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "edit emacs config")
   ;; add keybinding for reloading init.el file
   ;; "C-c r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "reload emacs config")
   "C-c r" '(reload-init-file :wk "reload emacs config")
   "C-s" 'swiper
   "C-c g" 'counsel-git
   "C-c j" 'counsel-git-grep
   "C-c k" 'counsel-rg
   "C-c f" 'counsel-recentf

   ;; vterm/esh
   "C-x x e" 'eshell
   "C-x x h" 'counsel-esh-history
   "C-c n" 'multi-vterm
   "C-c v" 'multi-vterm-dedicated-toggle

   ;; perspective
   "C-c p b" 'persp-ibuffer
   "C-c p s" 'persp-switch
   "C-x b" 'persp-counsel-switch-buffer
   ))

(setq make-backup-files nil)
(setq auto-save-default nil)

(use-package company
  :ensure t
  :diminish
  :custom
  (company-idle-delay .1)
  (company-minimum-prefix-length 1)
  (global-company-mode t))

(use-package diminish)

;; remove startup message when Emacs loads
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)

;; Emacs GUI improvements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(global-visual-line-mode 1)
(setq display-line-numbers-type 'relative)
(set-default 'truncate-lines t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(delete-selection-mode 1) ;; can select text and delete it by typing
(global-auto-revert-mode t) ;; automatically show changes if the file has changed

(electric-pair-mode 1) ;; turns on automatic parens when pairing
;; the next code prevents <> from auto-pairing when electric-pair-mode is on
;; otherwise, org-tempo is broken when you try to <s <TAB>...
(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       `(lambda (c)
                                          (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook 'eglot-ensure)
(add-hook 'prog-mode-hook 'company-mode)
;; fill columns
;; (setq-default fill-column '80)
;; (global-display-fill-column-indicator-mode 1)

(use-package flycheck
  :ensure t
  :after seq
  :diminish
  :init (global-flycheck-mode))

(use-package magit
  :ensure t)

(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package counsel-projectile
  :after ivy
  :config (counsel-projectile-mode))

(use-package ivy
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  )

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(use-package multi-vterm
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-tempo)

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package projectile
  :diminish
  :config (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 5000))

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(use-package ef-themes
  :ensure t
  :init (load-theme 'ef-light t)
  )

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :config
  ;; config setup from DistroTube
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil)
  )
