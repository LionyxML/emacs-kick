;;; init.el --- Emacs-Kicks --- A feature rich Emacs config for (neo)vi(m)mers -*- lexical-binding: t; -*-

;;; Commentary:
;; - [ ] Ask for permission on official kickstarter.nvim
;; - [ ] Intro doc
;; - [ ] Assumption that user already knows vim
;; - [ ] Document init.el
;; - [ ] Create README.org
;; - [ ] Create optional nerd fonts option
;; - [ ] Decide either nerdtree or treemacs or nothing (just dired)...


;; =====================================================================
;; ==================== READ THIS BEFORE CONTINUING ====================
;; =====================================================================
;; ========                                    .-----.          ========
;; ========         .----------------------.   | === |          ========
;; ========         |.-""""""""""""""""""-.|   |-----|          ========
;; ========         ||                    ||   | === |          ========
;; ========         ||     EMACS KICK     ||   |-----|          ========
;; ========         ||                    ||   | === |          ========
;; ========         ||                    ||   |-----|          ========
;; ========         ||M-x                 ||   |:::::|          ========
;; ========         |'-..................-'|   |____o|          ========
;; ========         `"")----------------(""`   ___________      ========
;; ========        /::::::::::|  |::::::::::\  \ no mouse \     ========
;; ========       /:::========|  |==hjkl==:::\  \ required \    ========
;; ========      '""""""""""""'  '""""""""""""'  '""""""""""'   ========
;; ========                                                     ========
;; =====================================================================
;; =====================================================================

;; What is Emacs Kick?
;;
;;   Emacs Kick is *not* a distribution.
;;
;;   Emacs Kick is a starting point for your own configuration.
;;     The goal is that you can read every line of code, top-to-bottom, understand
;;     what your configuration is doing, and modify it to suit your needs.
;;
;;     Once you've done that, you can start exploring, configuring and tinkering to
;;     make Neovim your own! That might mean leaving Emacs Kick just the way it is for a while
;;     or immediately breaking it into modular pieces. It's up to you!
;;
;;     If you don't know anything about Lua, I recommend taking some time to read through
;;     a guide. One possible example which will only take 10-15 minutes:
;;       - https://learnxinyminutes.com/docs/lua/
;;
;;     After understanding a bit more about Lua, you can use `:help lua-guide` as a
;;     reference for how Neovim integrates Lua.
;;     - :help lua-guide
;;     - (or HTML version): https://neovim.io/doc/user/lua-guide.html
;;
;; Emacs Kick Guide:
;;
;;   TODO: The very first thing you should do is to run the command `:Tutor` in Neovim.
;;
;;     If you don't know what this means, type the following:
;;       - <escape key>
;;       - :
;;       - Tutor
;;       - <enter key>
;;
;;     (If you already know the Neovim basics, you can skip this step.)
;;
;;   Once you've completed that, you can continue working through **AND READING** the rest
;;   of the kickstart init.lua.
;;
;;   Next, run AND READ `:help`.
;;     This will open up a help window with some basic information
;;     about reading, navigating and searching the builtin help documentation.
;;
;;     This should be the first place you go to look when you're stuck or confused
;;     with something. It's one of my favorite Neovim features.
;;
;;     MOST IMPORTANTLY, we provide a keymap "<space>sh" to [s]earch the [h]elp documentation,
;;     which is very useful when you're not exactly sure of what you're looking for.
;;
;;   I have left several `:help X` comments throughout the init.lua
;;     These are hints about where to find more information about the relevant settings,
;;     plugins or Neovim features used in Emacs Kick.
;;
;;    NOTE: Look for lines like this
;;
;;     Throughout the file. These are for you, the reader, to help you understand what is happening.
;;     Feel free to delete them once you know what you're doing, but they should serve as a guide
;;     for when you are first encountering a few different constructs in your Neovim config.
;;
;; If you experience any errors while trying to install kickstart, run `:checkhealth` for more info.
;;
;; I hope you enjoy your Neovim journey,
;; - TJ
;;
;; P.S. You can delete this when you're done too. It's your config now! :)


;;; Code:

;;  Some performance Hacks
(setq gc-cons-threshold #x40000000)
(setq read-process-output-max (* 1024 1024 4))


;; Adding the MELPA packages repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defcustom ek-use-nerd-fonts t
  "Configuration for using Nerd Fonts Symbols."
  :type 'boolean
  :group 'appearance)

;;; EMACS
(use-package emacs
  :ensure nil
  :bind
  (("M-o" . other-window)
   ("C-x C-b" . ibuffer))
  :custom
  (column-number-mode t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode 1)
  (display-line-numbers-type 'relative)
  (global-auto-revert-non-file-buffers t)
  (history-length 25)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ispell-dictionary "en_US")
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (split-width-threshold 300)
  (switch-to-buffer-obey-display-actions t)
  (switch-to-prev-buffer-skip-regexp "\*[^*]+\*")
  (switch-to-next-buffer-skip-regexp "\*[^*]+\*")
  (tab-always-indent 'complete) ;; TAB serves as M-TAB to completion
  (tab-width 4)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  :hook
  (prog-mode . display-line-numbers-mode)
  :config

  ;; TODO: themessss
  (load-theme 'modus-vivendi-tinted)

  ;; TODO: explain transparency
  (defun on-after-init ()
	(unless (display-graphic-p (selected-frame))
	  (set-face-background 'default "unspecified-bg" (selected-frame))))
  (add-hook 'window-setup-hook 'on-after-init)

  ;; Configure fonts per OS
  ;; Explain GUI
  (set-face-attribute 'default nil :height 100)
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (set-face-attribute 'default nil :height 140))

  ;; Save manual customizations to other file than init.el
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when scroll-bar-mode
    (scroll-bar-mode -1))

  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode)
  (xterm-mouse-mode 1)
  (file-name-shadow-mode 1)

  (modify-coding-system-alist 'file "" 'utf-8)
  
  (select-frame-set-input-focus (selected-frame))
  (toggle-frame-maximized)
 
  (add-hook 'after-init-hook
    (lambda ()
      (message "Emacs has fully loaded. This code runs after startup.")

      (with-current-buffer (get-buffer-create "*scratch*")
        (insert (format "
;;    Loading time : %s
;;    Packages     : %s
"
                  (emacs-init-time)
                  (number-to-string (length package-activated-list)))))))

  )

;;; WINDOW
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
	 ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))
	 
     ;; ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . 0))

     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
	 
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
	 )))

;;; DIRED
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah --group-directories-first")
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "open" "xdg-open")))
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls)))))


;;; ERC
(use-package erc
  :defer t
  :custom
  (erc-join-buffer 'window)
  ;; (erc-interactive-display ...) ;; this option will be available on next ERC release (5.6)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-timestamp-format "[%H:%M]")
  (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs"))))


;; ISEARCH
(use-package isearch
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?"))


;;; VC
;; Notes about why this is here and why magit is better for git...
(use-package vc
  ;; This is not needed, but it is left here as a reminder of some of the keybindings
  :defer t
  :bind
  (("C-x v d" . vc-dir)
   ("C-x v =" . vc-diff)
   ("C-x v D" . vc-root-diff)
   ("C-x v v" . vc-next-action)))


;;; SMERGE
(use-package smerge-mode
  :defer t
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-previous)))


;;; ELDOC
(use-package eldoc
  :init
  (global-eldoc-mode))


;;; FLYMAKE
(use-package flymake
  :defer t
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))


;;; ORG-MODE
(use-package org
  :defer t)


;;; --------------- External packages
(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode)
  :custom
  (vertico-count 10)                    ; Number of candidates to display
  (vertico-resize nil)
  (vertico-cycle nil)                   ; Go from last to first candidate and first to last (cycle)?
  :config
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
    (lambda (orig cand prefix suffix index _start)
      (setq cand (funcall orig cand prefix suffix index _start))
      (concat
        (if (= vertico--index index)
          (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
          "  ")
        cand))))


(use-package orderless
  :ensure t
  :defer t
  :after vertico
  :init
   (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package marginalia
  :ensure t
  :hook
  (after-init . marginalia-mode))


(use-package consult
  :ensure t
  :defer t
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))


;; EMBARK
(use-package embark
  :ensure t
  :defer t)


(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; TreeSitter Grammars
(use-package treesit-auto
  :ensure t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))


;; Markdown Mode
(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;; Company
(use-package company
  :defer t
  :ensure t
  :custom
  (company-tooltip-align-annotations t)
  :config
  (define-key company-active-map (kbd "C-y") (lambda () (interactive) (company-show-doc-buffer t)))
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map [ret] 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  :hook
  (after-init . global-company-mode))


;; LSP
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (bash-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (js-mode . lsp)
         (js-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-inlay-hint-enable t)
  (lsp-completion-provider :none)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil) ;; for speed
  (lsp-idle-delay 0) ;; debouncing, if needed 0.5
  (lsp-keep-workspace-alive nil)
  ;; core
  (lsp-enable-xref t)
  (lsp-auto-configure t)
  (lsp-enable-links nil)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-dap-auto-configure t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'/ `flymake'
  (lsp-modeline-workspace-status-enable t) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-eldoc-render-all t)
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet nil)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; lens
  (lsp-lens-enable t)
  ;; headerline
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)
  (lsp-headerline-arrow "▶")
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; semantic
  (lsp-semantic-tokens-enable nil))


;; LSP aditional servers
(use-package lsp-tailwindcss
  :ensure t
  :defer t
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html"))
  :init
  (setq lsp-tailwindcss-add-on-mode t))


;; Diff-HL
(use-package diff-hl
  :defer t
  :ensure t
  :hook
  (find-file . (lambda ()
                 (global-diff-hl-mode)
                 (diff-hl-flydiff-mode)
                 (diff-hl-margin-mode)))
  :custom
  (diff-hl-side 'left)
  :bind
  (("M-9" . 'diff-hl-previous-hunk)
   ("M-0" . 'diff-hl-next-hunk)))


;; Magit
(use-package magit
  :ensure t
  :defer t)


;; Minions
(use-package minions
  :defer t
  :ensure t
  :hook
  (after-init . minions-mode)
  :config
  ;; Set the icon or text shown for hidden minor modes
  (setq minions-mode-line-lighter "λ")  ;; You can change this to any icon or text

  ;; List of modes to display directly in the modeline
  (setq minions-prominent-modes
        '(flymake-mode
		  lsp-mode)))


;; Which-Key
(use-package which-key
  :ensure t
  :defer t
  :hook
  (after-init . which-key-mode))


;; XCLIP
(use-package xclip
  :ensure t
  :defer t
  :hook
  (after-init . xclip-mode))


(use-package indent-guide
  :defer t
  :ensure t
  :hook
  (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "│"))


;; Add-Node-Modules-Path
(use-package add-node-modules-path
  :ensure t
  :defer t
  :custom
  ;; Makes sure you are using the local bin for your
  ;; node project. Local eslint, typescript server...
  (eval-after-load 'typescript-ts-mode
	'(add-hook 'typescript-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'tsx-ts-mode
	'(add-hook 'tsx-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescriptreact-mode
	'(add-hook 'typescriptreact-mode-hook #'add-node-modules-path))
  (eval-after-load 'js-mode
	'(add-hook 'js-mode-hook #'add-node-modules-path)))


;; EVIL
(use-package evil
  :ensure t
  :defer t
  :hook
  (after-init . evil-mode)
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  ;; Set the leader key to space
  (setq evil-want-leader t)
  (setq evil-leader/in-all-states t)
  (setq evil-undo-system 'undo-tree)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  ;; Define leader key bindings
  (evil-define-key 'normal 'global (kbd "C-d") 'scroll-up)
  (evil-define-key 'normal 'global (kbd "C-u") 'scroll-down)
  (evil-define-key 'normal 'global (kbd "<leader> s f") 'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader> s g") 'consult-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s G") 'consult-git-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s r") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader> s h") 'consult-info)
  (evil-define-key 'normal 'global (kbd "<leader> /") 'consult-line)

  (evil-define-key 'normal 'global (kbd "<leader> x x") 'consult-flymake)
  (evil-define-key 'normal 'global (kbd "] d") 'flymake-goto-next-error)
  (evil-define-key 'normal 'global (kbd "[ d") 'flymake-goto-prev-error)

  (evil-define-key 'normal 'global (kbd "] c") 'diff-hl-next-hunk)
  (evil-define-key 'normal 'global (kbd "[ c") 'diff-hl-previous-hunk)

  (evil-define-key 'normal 'global (kbd "<leader> e e") 'neotree-toggle)

  (evil-define-key 'normal 'global (kbd "<leader> g g") 'magit-status)
  (evil-define-key 'normal 'global (kbd "<leader> g l") 'magit-log-current)
  (evil-define-key 'normal 'global (kbd "<leader> g d") 'magit-diff-buffer-file)

  (evil-define-key 'normal 'global (kbd "] b") 'switch-to-next-buffer)
  (evil-define-key 'normal 'global (kbd "[ b") 'switch-to-prev-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b i") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b d") 'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b k") 'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b x") 'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b s") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b l") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'consult-buffer)

  (evil-define-key 'normal 'global (kbd "<leader> p b") 'consult-project-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> p p") 'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader> p f") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader> p g") 'project-find-regexp)
  (evil-define-key 'normal 'global (kbd "<leader> p k") 'project-kill-buffers)
  (evil-define-key 'normal 'global (kbd "<leader> p D") 'project-dired)

  (evil-define-key 'normal 'global (kbd "P") 'consult-yank-from-kill-ring)

  (evil-define-key 'normal 'global (kbd "<leader> .") 'embark-act)

  (evil-define-key 'normal 'global (kbd "<leader> u") 'undo-tree-visualize)

  (evil-define-key 'normal 'global (kbd "<leader> h m") 'describe-mode)
  (evil-define-key 'normal 'global (kbd "<leader> h f") 'describe-function)
  (evil-define-key 'normal 'global (kbd "<leader> h v") 'describe-variable)
  (evil-define-key 'normal 'global (kbd "<leader> h k") 'describe-key)

  (evil-define-key 'normal 'global (kbd "] t") 'tab-next)
  (evil-define-key 'normal 'global (kbd "[ t") 'tab-previous)

  (evil-define-key 'normal 'global (kbd "<leader> m p") (lambda ()
														  (interactive)
														  (shell-command (concat "prettier --write " (shell-quote-argument (buffer-file-name))))
														  (revert-buffer t t t)))

  (evil-define-key 'normal lsp-mode-map
	;; (kbd "gd") 'lsp-find-definition                ;; Emacs already provides a better gd
	;; (kbd "gr") 'lsp-find-references                ;; Emacs already provides a better gr
	(kbd "K") 'lsp-describe-thing-at-point            ;; Show hover documentation
	(kbd "<leader> c a") 'lsp-describe-thing-at-point ;; Show hover documentation
	(kbd "<leader> r n") 'lsp-rename                  ;; Rename symbol
	(kbd "gI") 'lsp-find-implementation               ;; Find implementation
	(kbd "<leader> l f") 'lsp-format-buffer)          ;; Format buffer via lsp


   

  (evil-define-key 'normal 'global (kbd "gcc")
    (lambda ()
      (interactive)
      (if (not (use-region-p))
          (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  (evil-define-key 'visual 'global (kbd "gc")
    (lambda ()
      (interactive)
      (if (use-region-p)
          (comment-or-uncomment-region (region-beginning) (region-end)))))

  (evil-mode 1))


;; EVIL COLLECTION
(use-package evil-collection
  :after evil
  :defer t
  :ensure t
  :hook
  (evil-mode . evil-collection-init)
  :config)

(use-package undo-tree
  :defer t
  :ensure t
  :hook
  (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))

;; RAINBOWN DELIMITERS
(use-package rainbow-delimiters
  :defer t
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config)


(use-package doom-modeline
  :ensure t
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  :config
  (if ek-use-nerd-fonts
    (setq doom-modeline-icon t)
    (setq doom-modeline-icon nil))
  :hook
  (after-init . doom-modeline-mode))


(use-package neotree
  :ensure t
  :custom
  (neo-theme 'nerd)
  (neo-vc-integration '(face char))
  :defer t
  :config
  (if ek-use-nerd-fonts
    (setq neo-theme 'nerd-icons)
    (setq neo-theme 'nerd)))


(use-package nerd-icons
  :if ek-use-nerd-fonts
  :ensure t
  :defer t)

(use-package nerd-icons-dired
  :if ek-use-nerd-fonts
  :ensure t
  :defer t
  :hook
  (dired-mode . nerd-icons-dired-mode))



;; Utilitary function to install Emacs-Kicks
(defun ek/first-install ()
  "Install tree-sitter grammars and compile packages on first run..."
  (interactive)
  (switch-to-buffer "*Messages*")

  (message ">>> All required packages installed.")
  (message ">>> Configuring LEmacs...")

  (message ">>> Configuring Tree Sitter parsers...")
  (require 'treesit-auto)
  (treesit-auto-install-all)

  (message ">>> Native compile 3rd-party packages...\n")
  (require 'comp)
  (native-compile-prune-cache)
  (dolist (dir (directory-files package-user-dir t "^[^.]" t))
    (when (file-directory-p dir)
      (byte-recompile-directory dir 0 t)
      (native-compile-async dir 'recursively)))
 
  (message ">>> Emacs-Kicks installed!!! Presss any key to close the installer and open Emacs normally.")
  (read-key)
  (kill-emacs))


(provide 'init)
;;; init.el ends here
