(require 'package)
;(setq url-proxy-services
;   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;     ("http" . "proxy.bloomberg.com:81")
;     ("https" . "proxy.bloomberg.com:81")))

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



;; IDO Mode
;;(require 'ido)
;;(ido-mode t)
