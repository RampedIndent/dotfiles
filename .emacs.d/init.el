(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq dw/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(setq inhibit-start-message t)

(unless dw/is-termux
        (scroll-bar-mode -1)        ; Disable visible scrollbar
        (tool-bar-mode -1)          ; Disable the toolbar
        (tooltip-mode -1)           ; Disable tooltips
        (set-fringe-mode 10))       ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar
;;(setq default-frame-alist '((undecorated . t)))
;; Set up the visible bell
(setq visible-bell t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Configure Emacs Backup files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(load-theme 'tango-dark)

(unless dw/is-termux
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq use-dialog-box nil)) ;; Disable dialog boxes since they weren't working in Mac OSX
(use-package fast-scroll
  :straight t
  :config
  (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
  (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
  (fast-scroll-config)
  (fast-scroll-mode 1)
  )

;; Initalize package sources
(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                                                 ;;("org" . "https://orgmode.org/elpa/")
                                                 ("elpa" . "https://elpa.gnu.org/packages/")))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initalize use package on non linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; (setq use-package-always-ensure t)

(use-package command-log-mode)
(use-package swiper)


(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         )
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  )

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))
(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package wgrep)

(use-package ivy-posframe
  :disabled
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

(require 'server)
(unless (server-running-p)
    (server-start))

(use-package all-the-icons
  :straight t
  ;; :config
  ;; (all-the-icons-install-fonts)
  )
(use-package unicode-fonts
  :straight t
  :config
  (unicode-fonts-setup))
;;(set-face-attribute 'default nil :font "DejaVu Sans Mono")
;; (set-face-attribute 'heading-variable-pitch nil 
;;                     :font "Signika Negative"
;;                     :height 1.6
;;                     :weight 'extra-light
;;                     :width  'ultra-expanded )

(set-face-attribute 'variable-pitch nil 
                    :font "Signika Negative"
                    :height 1.6
                    :weight 'extra-light
                    :width  'ultra-expanded )
(set-face-attribute 'fixed-pitch nil 
                    :font "Inconsolata Go Nerd Font"
                    :height 0.8)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "opera")

(defun efs/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(column-number-mode)
;; (global-display-line-numbers-mode t)

(setq display-line-numbers-type 'relative)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'org-src-mode-hook 'display-line-numbers-mode)
(add-hook 'rustic-mode-hook 'display-line-numbers-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :init(rainbow-delimiters-mode t))

(electric-pair-mode t)

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
    (progn
      (if (derived-mode-p 'prog-mode)
        (progn
          (message "Flyspell on (code)")
          (flyspell-prog-mode))
        ;; else
        (progn
          (message "Flyspell on (text)")
          (flyspell-mode 1)))
      ;; I tried putting (flyspell-buffer) here but it didn't seem to work
      )))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
      ; else - flyspell is off, turn it on
      (flyspell-on-for-buffer-type)))

(use-package wc-mode
  :straight t
  :init
  (add-to-list 'global-mode-string '("" wc-buffer-stats)))

(use-package writegood-mode
  :straight t)

(use-package evil-nerd-commenter
:bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package smart-newline
  :config  
  (dolist (mode '(
                org-mode-hook
                rustic-mode-hook
                ))
  (add-hook mode (lambda () (smart-newline-mode 1))))

  )

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/documents/Projects/Code")
    (setq projectile-project-search-path '("~/documents/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))
(use-package counsel-projectile
    :straight t)

(defun cust-git-pull (&rest _args)
  (magit-pull-from-pushremote nil)
  )
(defun cust-git-push (&rest _args)
  (magit-push-current-to-pushremote nil)
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (global-set-key (kbd "<ESCAPE>") 'magit-dispatch)
  :config
  (advice-add 'magit :after 'cust-git-pull)
  ;; (advice-add 'magit-commit :after 'cust-git-push)
  (setq magit-post-commit-hook 'cust-git-push)
  )
(add-hook 'with-editor-mode-hook 'evil-insert-state)
  (setq magit-post-commit-hook 'cust-git-push)
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show)

;; (cust-git-pull)

(use-package perspective
  :straight t
  :bind(("C-x k" . persp-kill-buffer*))
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  :config 
  (persp-mode)
  )

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package multi-vterm
  :straight t)

(use-package all-the-icons-dired)

(use-package dired
  :straight nil
  ;;:straight nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (unless (or dw/is-termux
                          (s-equals? "/gnu/store/" (expand-file-name default-directory)))
                (all-the-icons-dired-mode 1))
              (hl-line-mode 1)))

  (use-package dired-rainbow
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

  (use-package dired-single
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  ;(evil-collection-define-key 'normal 'dired-mode-map
   ; "h" 'dired-single-up-directory
    ;"H" 'dired-omit-mode
   ; "l" 'dired-single-buffer
   ; "y" 'dired-ranger-copy
   ; "X" 'dired-ranger-move
   ; "p" 'dired-ranger-paste)
   )
(setq dired-kill-when-opening-new-dired-buffer t)
;; (defun dw/dired-link (path)
;;   (lexical-let ((target path))
;;     (lambda () (interactive) (message "Path: %s" target) (dired target))))

;; (dw/leader-key-def
;;   "d"   '(:ignore t :which-key "dired")
;;   "dd"  '(dired :which-key "Here")
;;   "dh"  `(,(dw/dired-link "~") :which-key "Home")
;;   "dn"  `(,(dw/dired-link "~/Notes") :which-key "Notes")
;;   "do"  `(,(dw/dired-link "~/Downloads") :which-key "Downloads")
;;   "dp"  `(,(dw/dired-link "~/Pictures") :which-key "Pictures")
;;   "dv"  `(,(dw/dired-link "~/Videos") :which-key "Videos")
;;   "d."  `(,(dw/dired-link "~/.dotfiles") :which-key "dotfiles")
;;   "de"  `(,(dw/dired-link "~/.emacs.d") :which-key ".emacs.d"))

(use-package dashboard
  :straight t
  :init
  (progn
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

    ;(setq dashboard-startup-banner "~/dotfiles/banner.png")

    (setq dashboard-items '((recents  . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5)
                            ;; (registers . 5)
                            ))
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-navigator t)
    (setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Org Roam Ui"
         "Insert hover Text"
         (lambda (&rest _) (browse-url "http://localhost:35901")))
          (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Syncthing"
         "Insert hover Text"
         (lambda (&rest _) (browse-url "http://localhost:8384")))
         ;; line 2
        ;; ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
        ;;   "Linkedin"
        ;;   ""
        ;;   (lambda (&rest _) (browse-url "homepage"))))
         )))
    (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
    )
  :config
  (dashboard-setup-startup-hook))


(defun dashboard-insert-custom (list-size)
  (dashboard-insert-heading "Journal")
  (insert "\n")
  (widget-create 'push-button 
                 :value "Dailies Capture Yesterday"
                 :format "    %[%v%]\n"
                 :notify (lambda (widget &rest ignore)
                            (org-roam-dailies-capture-yesterday 1)))
  (widget-create 'push-button 
                 :value "Dailies Capture Today"
                 :format "    %[%v%]\n"
                 :notify (lambda (widget &rest ignore)
                            (org-roam-dailies-capture-today)))
  (widget-create 'push-button 
                 :value "Dailies Capture Tomorrow"
                 :format "    %[%v%]\n"
                 :notify (lambda (widget &rest ignore)
                            (org-roam-dailies-capture-tomorrow 1)))

 (widget-create 'push-button 
                 :value "Dailies Capture Date"
                 :format "    %[%v%]\n"
                 :notify (lambda (widget &rest ignore)
                            (org-roam-dailies-capture-date)))
  )
 ;; ((insert "org-roam-dailies-capture-date"))
(add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
(add-to-list 'dashboard-items '(custom) t)

(unless t

  (use-package kubernetes
    :straight t
    :commands (kubernetes-overview)
    ;:config
    ;(setq kubernetes-poll-frequency 3600
     ;     kubernetes-redraw-frequency 3600))
    ))

(use-package treemacs
  :straight t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :straight t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :straight t
  :config (treemacs-set-scope-type 'Tabs))

(use-package edit-server
:straight t
:if window-system
:commands edit-server-start
:init (if after-init-time
            (edit-server-start)
          (add-hook 'after-init-hook
                    #'(lambda() (edit-server-start))))
:config (setq edit-server-new-frame-alist
              '((name . "Edit with Emacs FRAME")
                (top . 200)
                (left . 200)
                (width . 80)
                (height . 25)
                (minibuffer . t)
                (menu-bar-lines . t)
                (window-system . x))))

(use-package helm) 
(use-package helm-ag)

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete))
  )

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(defun rk/open-compilation-buffer (&optional buffer-or-name shackle-alist shackle-plist)
  "Helper for selecting window for opening *compilation* buffers."
  ;; find existing compilation window left of the current window or left-most window
  (let ((win (or (loop for win = (if win (window-left win) (get-buffer-window))
                       when (or (not (window-left win))
                                (string-prefix-p "*compilation" (buffer-name (window-buffer win))))
                       return win)
                 (get-buffer-window))))
    ;; if the window is dedicated to a non-compilation buffer, use the current one instead
    (when (window-dedicated-p win)
      (let ((buf-name (buffer-name (window-buffer win))))
        (unless (string-prefix-p "*compilation" buf-name)
          (setq win (get-buffer-window)))))
    (set-window-buffer win (get-buffer buffer-or-name))
    (set-frame-selected-window (window-frame win) win)))


(use-package shackle
  :ensure
  :diminish
  :custom
  (shackle-rules '((compilation-mode :custom rk/open-compilation-buffer :select t)
                   ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
                   ("\\*magit" :regexp t :same t :select t)
                   ("\\*shell.*" :regexp t :same t :select t)
                   ("\\*PowerShell.*" :regexp t :same t :select t)
                   ("\\*Cargo.*" :regexp t :other t :select nil)
                   ("*Messages*" :select nil :other t)
                   ("*go-guru-output*" :select t :same t)
                   ("*Proced*" :select t :same t)
                   ("*Buffer List*" :select t :same t)
                   ("\\*Pp Eval" :regexp t :same nil :select t :other t)
                   ("*Messages*" :same nil :other t :select t :inhibit-window-quit t)

                   ;; slime
                   ("*slime-source*" :select nil :same nil :other t)
                   ("*slime-description*" :select nil :other t :inhibit-window-quit t)
                   ("\\*slime-repl" :regexp t :same nil :select nil :other t)
                   ;; ("\\*sldb" :regexp t :other t :inhibit-window-quit t :select t)
                   ("\\*slime-compilation" :regexp t :same nil :select nil :other t)
                   ("*slime-scratch*" :same nil :select t :other t)

                   ;; ert
                   ("*ert*" :select nil :same nil :other t)

                   ;; clojure
                   ("*sesman CIDER browser*" :inhibit-window-quit t :select t :same t)
                   ("\\*cider-repl" :regexp t :same nil :other t)
                   ;; Org Babel

                   ("\\*Org Src*" :regexp t :same t :select t)

                   ))
  (shackle-default-rule nil))

(shackle-mode)

(use-package flycheck :ensure)

(use-package wakatime-mode
:config
(global-wakatime-mode)
(setq wakatime-api-key 
                         (efs/lookup-password :host "wakatime.com" :user "rampedindent-api")
                         )
)



(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :straight t)

(general-create-definer viktorya/editor-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC"
  )

(defun viktorya/org-insert-image-width ()
  (interactive)
  (insert "#+ATTR_HTML: :width 700")
)  
(viktorya/editor-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "tl" '(org-latex-preview :which-key "Toggle Latex Preview")
  "e" '(eval-buffer :which-key "Run the buffer")
  "g" '(magit :which-key "Runs Magit")
  "n" '(org-roam-node-find :which-key "Finds Node in Org Roam")
  "i" '(:ignore i :which-key "Insert commands")
  "in" '(org-roam-node-insert :which-key "Insert Org Roam Node Link")
  "ii" '(org-download-clipboard :which-key "Insert clipboard image into file")
  "iI" '(viktorya/org-insert-image-width :which-key "Insert image Width")
  "f" '(:ignore f :which-key "file commands")
  "ff" '(counsel-find-file :which-key "Find File")
  "fg" '(revert-buffer-no-confirm :which-key "Refresh File")
  "fs" '(save-buffer :which-key "Save Current Buffer")
  "fS" '(write-file :which-key "Save Current Buffer as")
  "w" '(:ignore w :which-key "file commands")
  "wv" '(evil-window-vsplit :which-key "Vertical Window Split")
  "wh" '(evil-window-split :which-key "Horizontal Window Split")
  "w <left>" '(evil-window-left :which-key "Move Active Window Left")
  "w <right>" '(evil-window-right :which-key "Move Active Window Right")
  "w <up>" '(evil-window-up :which-key "Move Active Window Up")
  "w <down>" '(evil-window-down :which-key "Move Active Window Down")
  "wq" '(evil-window-delete :which-key "Delete active window")
  ;;"p" '(yank :which-key "Paste Text")
  "b" '(:ignore b :which-key "Buffer commands")
  "bq" '(evil-delete-buffer :which-key "Delete the current buffer")
  "bb" '(counsel-switch-buffer :which-key "Buffer Switcher")
  "bt" '(treemacs :which-key "Toggle Treemacs")
  "v" '(multi-vterm :which-key "Start vterm")
  "<ESC>" '(evil-normal-state :which-key "Default Evil state")
  )

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))


(use-package hydra)

(defhydra hydra-text-scale (:timeout 10)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(viktorya/editor-keys
  "t" '(:ignore t :which-key "Text Commands")
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(viktorya/editor-keys
"o"   '(:ignore t :which-key "org mode")

"oi"  '(:ignore t :which-key "insert")
"oil" '(org-insert-link :which-key "insert link")
"oid" '(org-deadline :which-key "insert deadline")
"ois" '(org-schedule :which-key "insert sechedule")
"oie" '(org-set-effort :which-key "insert effort")
"oip" '(org-set-property :which-key "insert property")

"on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

;; "os"  '(dw/counsel-rg-org-files :which-key "search notes")

"os"  '(org-agenda :which-key "status")
"ot"  '(org-todo-list :which-key "todos")
"oc"  '(org-capture t :which-key "capture")
"ox"  '(org-export-dispatch t :which-key "export"))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(use-package doom-themes
  :init (load-theme 'custom-doom-outrun-electric t))

(require 'subr-x)
(unless dw/is-termux
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(use-package rainbow-mode)
(rainbow-mode t)

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "???"))))))
  (font-lock-add-keywords 'org-mode
                          '(("^[[:space:]]*\\(-\\) "
                             0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "???")))))
  (defun efs/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; (add-to-list 'org-modules 'org-habit)

(use-package org
  :pin elpa
  :hook (org-mode . efs/org-mode-setup)
  :config
  ;; (setq org-ellipsis " -")
  (setq org-ellipsis " ???")
  (efs/org-font-setup)
  (setq org-modules
        '(org-crypt
          org-habit
          org-bookmark
          org-habit
          org-eshell
          org-irc))
  (setq org-support-shift-select t)

  (setq org-habit-graph-column 60)

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  (setq org-startup-with-inline-images t)
  (setq org-agenda-files
        '("~/documents/syncthing/Todo Lists /school.org"
          "~/documents/syncthing/Todo Lists /Life.org"
          "~/documents/syncthing/Todo Lists /Emacs.org"
          "~/Documents/Notes/Habits.org"
          ;; "~/Documents/Notes/Org-Roam/"
          "~/documents/syncthing/Todo Lists /kubernetes.org"))
  (setq org-refile-targets
        '(("~/documents/syncthing/Todo Lists /Archive.org" :maxlevel . 2)
          ("~/documents/syncthing/Todo Lists /Life.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "MISSED(m)")))

  (setq org-image-actual-width nil)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . t)
  ;;    (ledger . t)))

  )

(require 'org-habit)
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  ;;:custom
  ;;(org-bullets-bullet-list '("???" "???" "???" "???" "???" "???" "???")
  )

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(setq org-capture-templates
`(;;("t" "Tasks / Projects")
  ;; ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
  ;;      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

  ;; ("j" "Journal Entries")
  ;; ("jj" "Journal" entry
  ;;      (file+olp+datetree "~/Documents/Notes/Journal.org")
  ;;      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
  ;;      ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
  ;;      :clock-in :clock-resume
  ;;      :empty-lines 1)
  ;; ("jm" "Meeting" entry
  ;;      (file+olp+datetree "~/Documents/Notes/Journal.org")
  ;;      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
  ;;      :clock-in :clock-resume
  ;;      :empty-lines 1)

  ;; ("w" "Workflows")
  ;; ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Documents/Notes/Journal.org")
  ;;      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

  ("m" "Metrics Capture")
  ("mw" "Weight" table-line (file+headline "~/Projects/Documents/Notes/Metrics.org" "Weight")
   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
  ))

(add-hook 'org-mode-hook (lambda ()
                           "Beautify Org Checkbox Symbol"
                           (push '("[ ]" .  "???") prettify-symbols-alist)
                           (push '("[X]" . "???" ) prettify-symbols-alist)
                           (push '("[-]" . "???" ) prettify-symbols-alist)
                           (prettify-symbols-mode)))
(defface org-checkbox-todo-text
  '((t (:inherit org-todo)))
  "Face for the text part of an unchecked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend))
 'append)

(defface org-checkbox-done-text
  '((t (:inherit org-done)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
 'append)

;; (defface org-checkbox-empty-text
;;   '((t (:foreground "#ff2afc" :strike-through nil)))
;;   "Face for the text part of a checked org-mode checkbox.")
;; (defface org-checkbox-done-text
;;   '((t (:foreground "#a7da1e" :strike-through nil)))
;;   "Face for the text part of a checked org-mode checkbox.")

;; (font-lock-add-keywords
;;  'org-mode
;;  `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
;;     1 'org-checkbox-empty-text prepend))
;;  'append)
;; (font-lock-add-keywords
;;  'org-mode
;;  `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
;;     1 'org-checkbox-done-text prepend))
;;  'append)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (shell . t)))

(setq org-confirm-babel-evaluate nil)
(setq org-babel-python-command "python3")
(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ya" . "src yaml"))
(add-to-list 'org-structure-template-alist '("to" . "src toml :tangle ./"))
(add-to-list 'org-structure-template-alist '("conf" . "src conf :tangle ./"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("rst" . "src rustic"))

;; ;; Automatically tangle our Emacs.org config file when we save it
;; (defun efs/org-babel-tangle-config ()
;;   (when (or (string-equal (buffer-file-name)
;;                       (expand-file-name "~/.dotfiles/emacs.org"))
;;                       (string-equal (buffer-file-name)
;;                                     (expand-file-name "~/.dotfiles/system.org"))
;;                       )
;;     ;; Dynamic scoping to the rescue
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
(use-package org-auto-tangle
;; :load-path "site-lisp/org-auto-tangle/"    ;; this line is necessary only if you cloned the repo in your site-lisp directory 
:defer t
:hook (org-mode . org-auto-tangle-mode))

(setq org-auto-tangle-babel-safelist '(
                                       "/home/rampedindent/.dotfiles/system.org"
                                       "/home/rampedindent/tmp/test.org"
                                       ))

(setq org-auto-tangle-babel-safelist '(
                                     "/home/rampedindent/.dotfiles/system.org"
                                     "/home/rampedindent/tmp/test.org"
                                     ))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :straight t
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-roam-directory "~/Documents/Notes/Org-Roam/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-completion-system 'default)
  (setq org-roam-capture-templates
   '(("d" "default" plain "* ${title} \n%?"
      ;; #'org-roam-capture--get-point
      ;; "%?"
      ;; :file-name "%<%Y%m%d%H%M%S>-${slug}"
      ;; :head "#+title: ${title}\n"
      :target (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "default math" plain
      "#+STARTUP: latexpreview\n* ${title} \n%?"
      ;; #'org-roam-capture--get-point
      ;; "%?"
      ;; :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :target (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                         "#+title: ${title}\n")
      :unnarrowed t)

     ("ll" "link note" plain
      ;; #'org-roam-capture--get-point
      "* %^{Link}"
      :file-name "Inbox"
      :olp ("Links")
      :unnarrowed t
      :immediate-finish)
     ("lt" "link task" entry
      ;; #'org-roam-capture--get-point
      "* TODO %^{Link}"
      :file-name "Inbox"
      :olp ("Tasks")
      :unnarrowed t
      :immediate-finish)))
  (setq org-roam-dailies-directory "Journal/")
  (setq org-roam-dailies-capture-templates
   '(("d" "default" entry
      ;; #'org-roam-capture--get-point
      "* %?"
      ;; :file-name "Journal/%<%Y-%m-%d>"
      ;; :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n"
      :target (file+head "${slug}.org"
                         "#+title: ${title}\n"))

     ("t" "Task" entry
      ;; #'org-roam-capture--get-point
      "* TODO %?\n  %U\n  %a\n  %i"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Tasks")
      :empty-lines 1
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("j" "journal" entry
      ;; #'org-roam-capture--get-point
      "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("l" "log entry" entry
      ;; #'org-roam-capture--get-point
      "* %<%I:%M %p> - %?"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("m" "meeting" entry
      ;; #'org-roam-capture--get-point
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))

  (org-roam-update-org-id-locations)
  (org-roam-db-autosync-mode t)
  :bind (:map org-roam-mode-map
              (("C-c n l"   . org-roam)
               ("C-c n f"   . org-roam-find-file)
               ("C-c n d"   . org-roam-dailies-find-date)
               ("C-c n c"   . org-roam-dailies-capture-today)
               ("C-c n C r" . org-roam-dailies-capture-tomorrow)
               ("C-c n t"   . org-roam-dailies-find-today)
               ("C-c n y"   . org-roam-dailies-find-yesterday)
               ("C-c n r"   . org-roam-dailies-find-tomorrow)
               ("C-c n g"   . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-node-insert)))
  )

(use-package websocket
  :unless dw/is-termux
  :after org-roam)

(use-package org-roam-ui
  :unless dw/is-termux
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil)
  (unless (org-roam-ui-mode)
    (org-roam-ui-mode t))

  )

(use-package org-download
  :straight t)

(setq org-latex-create-formula-image-program 'dvipng)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(use-package ox-hugo
  :straight t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

  (with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

   (add-to-list 'org-capture-templates
               '("A"                ;`org-capture' binding + h
                 "Hugo Art post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "all-posts.org" "Art")
                 (function org-hugo-new-subtree-post-capture-template)))
 (add-to-list 'org-capture-templates
               '("T"                ;`org-capture' binding + h
                 "Hugo Tech post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "all-posts.org" "Tech")
                 (function org-hugo-new-subtree-post-capture-template)))

  )

;; (use-package ivy-xref
;;   :straight t
;;   :init (if (< emacs-major-version 27)
;;           (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
;;           (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package lsp-mode
  ;;:straight t
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  )

(viktorya/editor-keys
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(use-package lsp-ui
  ;;:straight t
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package lsp-treemacs
  :after lsp)
;; (use-package lsp-ivy
;;   :hook (lsp-mode . lsp-ivy-mode))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package rustic
  :straight t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  (advice-add 'cargo-process-run :before #'viktorya/save-buffer)
  )
(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(defun viktorya/save-buffer (&rest _args)
  (save-buffer 1)
  )

(use-package cargo
  :straight t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  )
