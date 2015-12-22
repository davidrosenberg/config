(require 'package)

(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "proxy.bloomberg.com:81")
     ("https" . "proxy.bloomberg.com:81")))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(use-package magit
  :disabled t
  :ensure magit)

(use-package elpy
  :init (elpy-enable)
  :ensure)

(use-package ess
  :commands R
  :ensure)


;; ESS Mode
(require 'ido)
(ido-mode t)


;deferred epc 
;        flycheck ctable jedi concurrent company cyberpunk-theme elpy 
;        yasnippet pyvenv highlight-indentation find-file-in-project 
;        sql-indent sql exec-path-from-shell iedit
;        auto-complete popup let-alist magit git-rebase-mode 
;        git-commit-mode minimap popup


;; WindMove
(require 'windmove)			; to load the package
(global-set-key (kbd "C-M-<left>")  'windmove-left)
(global-set-key (kbd "C-M-<up>")    'windmove-up)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<down>")  'windmove-down)
(global-set-key (kbd "C-M-b")  'windmove-left)
(global-set-key (kbd "C-M-p")    'windmove-up)
(global-set-key (kbd "C-M-f") 'windmove-right)
(global-set-key (kbd "C-M-n")  'windmove-down)
(setq windmove-wrap-around t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Remove unnecessary gui stuff
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 1)

;; Colors
(set-background-color "black")
(set-face-background 'default "black")
(set-face-background 'region "black")
(set-face-foreground 'default "white")
(set-face-foreground 'region "gray60")
(set-foreground-color "white")
(set-cursor-color "red")

;; IDO Mode
(require 'ido)
(ido-mode t)

;; RECENT FILES
(require 'recentf)			; Add recent opened files menu
(recentf-mode 1)
