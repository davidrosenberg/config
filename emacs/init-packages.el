(require 'package)

(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "proxy.bloomberg.com:81")
     ("https" . "proxy.bloomberg.com:81")))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(use-package magit
  :ensure magit)



;deferred epc 
;        flycheck ctable jedi concurrent company cyberpunk-theme elpy 
;        yasnippet pyvenv highlight-indentation find-file-in-project 
;        sql-indent sql exec-path-from-shell iedit
;        auto-complete popup let-alist magit git-rebase-mode 
;        git-commit-mode minimap popup
