;; ~/.emacs should be a symbolic link to this file
;; highlight something and do C-x C-e  to evalute in emacs!

;; Repository setup
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
;                         ("elpy" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Goal is for this to be a cross-platform .emacs file

(add-to-list 'load-path "~/emacs/")

;;;;Make sure that .emacs file is edited in lisp mode:
(setq auto-mode-alist (cons '("\.emacs" . lisp-mode) auto-mode-alist))


; Upon drag-and-drop: Find the file, w/shift insert filename; w/meta insert file contents
; note that the emacs window must be selected (CMD-TAB) for the modifiers to register
(define-key global-map [M-ns-drag-file] 'ns-insert-file)
(define-key global-map [S-ns-drag-file] 'ns-insert-filename)
(define-key global-map [ns-drag-file] 'ns-find-file-in-frame)

(defun ns-insert-filename ()
  "Insert contents of first element of `ns-input-file' at point."
  (interactive)
  (let ((f (pop ns-input-file)))
    (insert f))
  (if ns-input-file                     ; any more? Separate by " "
      (insert " ")))

(defun ns-find-file-in-frame ()
  "Do a `find-file' with the `ns-input-file' as argument; staying in frame."
  (interactive)
  (let ((ns-pop-up-frames nil))
    (ns-find-file)))


;; FONT & COLOR STUFF
(global-font-lock-mode t)
;;(if (>= emacs-major-version 23)
;;    (set-default-font "Inconsolata-13"))
;;(set-default-font "Menlo-13")


;; STUFF FOR SHELL (comint) [not sure what this does ;)]
(require 'comint)
(define-key comint-mode-map [(meta p)]
   'comint-previous-matching-input-from-input)
(define-key comint-mode-map [(meta n)]
   'comint-next-matching-input-from-input)
(define-key comint-mode-map [(control meta n)]
    'comint-next-input)
(define-key comint-mode-map [(control meta p)]
    'comint-previous-input)

;; other Shell stuff from http://snarfed.org/space/why_I_dont_run_shells_inside_Emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
)

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

;; MISC STUFF
(setq case-fold-search t) ; make searches case insensitive
(transient-mark-mode 1)		 ; Make regions (aka blocks) highlight
(setq inhibit-startup-message t) ; Don't show startup screen

;; Automatically decompress when opening gzipped files
(auto-compression-mode  1)

;; DIRED Stuff
;;(when (require 'dired-sync nil t)
;;   (define-key dired-mode-map (kbd "C-c S") 'dired-do-sync))

;; Sunrise Commander (two-pane file manager)
;;(require 'sunrise-commander)
;;(require 'sunrise-x-buttons)
;;(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))

;; 5) Type M-x sunrise to invoke the Sunrise Commander (or much better: bind the
;; function to your favorite key combination). The command `sunrise-cd' invokes
;; Sunrise and automatically selects the current file wherever it is in the
;; filesystem. Type h at any moment for information on available key bindings.

;; 6) Type M-x customize-group <RET> sunrise <RET> to customize options, fonts
;; and colors (activate AVFS support here, too).

;; Word wrap for text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; http://www.emacswiki.org/emacs/WhiteSpace
(require 'whitespace)


;; System-type and particular system customization
;; From http://sigquit.wordpress.com/2008/09/28/single-dot-emacs-file/
;; Get current system's name
(defun insert-system-name()
  (interactive)
  "Get current system's name"
  (insert (format "%s" system-name))
  )
;; Get current system type
(defun insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type))
  )
;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin")
  )
;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux")
  )

(if (system-type-is-darwin)
    (message "is darwin")
  )
(if (system-type-is-gnu)
    (message "is gnu")
  )

;; Mac OS X Customizations
(if (system-type-is-darwin)
    (progn ;; progn defines a statement block
      ;; Open files, each in a new buffer, when drag and dropping onto emacs
      (setq send-mail-function (quote mailclient-send-it))
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)
      (if nil
	  (define-key global-map [ns-drag-file] 'my-ns-open-files)
	(defun my-ns-open-files ()
	  "Open files in the list `ns-input-file'."
	  (interactive)
	  (mapc 'find-file ns-input-file)
	  (setq ns-input-file nil))

	;; Don't open new files in fresh window
	(setq ns-pop-up-frames nil)
	;; disable closing emacs in Mac OS X
	(global-unset-key "\C-z")
	;; Load Enhanced Carbon Emacs plugins
	;; ECE includes a bunch of packages with special customization for Mac
;;	(unless (or (boundp 'enhanced-carbon-emacs)
;;		    (boundp 'aquamacs-version))
;;	  (defun load-local-site-start (site-lisp-directory)
;;	    "Load site-start.el from a given site-lisp directory"
;;	    (let ((current-default-directory default-directory))
;;	      (setq default-directory site-lisp-directory)
;;	      (normal-top-level-add-subdirs-to-load-path)
;;	      (setq default-directory current-default-directory)
;;	      (setq load-path (cons site-lisp-directory load-path))
;;	      (load (concat site-lisp-directory "/site-start.el"))
;;	      ))
;;	   (load-local-site-start "/Library/Application Support/emacs/ec-emacs/site-lisp")
;;	  (load-local-site-start "~/emacs/ec-emacs/site-lisp")
;;	  )
	)

      )
  )

;; GNU/Linux Customizations
(if (system-type-is-gnu)
    (progn
;;      (set-default-font "schumacher-clean-medium-r-normal--12-*-75-75-c-60-iso10646-1")
      (set-default-font "-misc-fixed-medium-r-normal--14-*-75-75-c-70-iso8859-1")
      )
  )

;; Machine-Name Specific Customizations

;; Scala Mode
(add-to-list 'load-path "~/emacs/scala-mode")
(require 'scala-mode-auto)
;; expand tabs into spaces
(defun me-turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'scala-mode-hook 'me-turn-off-indent-tabs-mode)


;; OCTAVE STUFF
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; MATLAB STUFF
(add-to-list 'load-path "~/emacs/matlab-elisp")
(autoload 'matlab-eei-connect "matlab-eei"
  "Connects Emacs to MATLAB's external editor interface.")
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;;(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
;; User Level customizations (You need not use them all):
(setq matlab-indent-function-body t) ; if you want function bodies indented
(setq matlab-verify-on-save-flag nil)	; turn off auto-verify on save
(defun my-matlab-mode-hook ()
  (setq fill-column 76))		; where auto-fill should wrap
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
(defun my-matlab-shell-mode-hook ()
  '())
(add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

;; BUFFER SWITCHING STUFF  (IDO mode seems better since activates for find-file as well)
;;(iswitchb-mode 1)


;; ESS Stuff
					; http://www.emacswiki.org/cgi-bin/wiki/EmacsSpeaksStatistics
(setq load-path (cons "~/emacs/ESS/lisp" load-path) )
;; ESS-Stuff (e.g. shift-enter execute)
;;(ess-toggle-underscore nil)
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
	(delete-other-windows)
	(setq w1 (selected-window))
	(setq w1name (buffer-name))
	(setq w2 (split-window w1))
	(R)
	(set-window-buffer w2 "*R*")
	(set-window-buffer w1 w1name))))
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))
(add-hook 'ess-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'my-ess-eval)))
(add-hook 'inferior-ess-mode-hook
	  '(lambda()
	     (local-set-key [C-up] 'comint-previous-input)
	     (local-set-key [C-down] 'comint-next-input)))
(require 'ess-site)
(require 'r-utils)

;; ESSH stuff
(require 'essh)
(defun essh-sh-hook ()                                             ;;
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)        ;;
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)        ;;
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)          ;;
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step) ;;
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)      ;;
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory)) ;;
 (add-hook 'sh-mode-hook 'essh-sh-hook)

;; Latex stuff
(add-hook 'LaTeX-mode-hook
	  (function (lambda ()
		      (paren-toggle-matching-quoted-paren 1)
		      (paren-toggle-matching-paired-delimiter 1))))


;; AucTeX
;;(add-to-list 'load-path "~/emacs/AUCTEX")
;;(add-to-list 'load-path "~/emacs/AUCTEX/preview")
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Sweave / AucTeX / ESS
(defun Rnw-mode ()
  (require 'ess-noweb)
  (noweb-mode)
  (if (fboundp 'R-mode)
      (setq noweb-default-code-mode 'R-mode)))

(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
(setq reftex-file-extensions
      '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))

(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Snw-mode))
(add-hook 'Rnw-mode-hook
	  (lambda ()
	    (add-to-list 'TeX-command-list
			 '("Sweave" "R CMD Sweave %s"
			   TeX-run-command nil (latex-mode) :help "Run Sweave") t)
	    (add-to-list 'TeX-command-list
			 '("LatexSweave" "%l %(mode) %s"
			   TeX-run-TeX nil (latex-mode) :help "Run Latex after Sweave") t)
	    (setq TeX-command-default "LatexSweave")))


;; Linking ESS with AucTex
;(add-hook 'Rnw-mode-hook
;          (lambda ()
;            (add-to-list 'TeX-command-list
;                         '("Sweave" "R CMD Sweave %s"
;                           TeX-run-command nil t :help "Run Sweave") t)
;            (add-to-list 'TeX-command-list
;                         '("LatexSweave" "%l \"%(mode)\\input{%s}\""
;                           TeX-run-TeX nil t :help "Run Latex after Sweave") t)
;            (setq TeX-command-default "Sweave")))


;; RECENT FILES
(require 'recentf)			; Add recent opened files menu
(recentf-mode 1)


;; ORG MODE
;;(setq load-path (cons "~/emacs/ORG/lisp" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


;; eshell stuff
(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
	(beginning-of-line))))

(add-hook 'eshell-mode-hook
	  '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))

;; PYTHON STUFF
;; (setq ipython-command "/SOME-PATH/ipython")   ;; only needed if default ipython is wrong
;;(require 'ipython)

;; ICICLES
;(add-to-list 'load-path "~/emacs/icicles")

;; MAN PAGES
(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))

;; XML Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pretty print xml region
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    ;; split <foo><foo> or </foo><foo>, but not <foo></foo>
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (incf end))
    (indent-region begin end nil)
    (normal-mode))
  (message "All indented!"))

(setq mail-user-agent 'message-user-agent)
(setq mail-default-headers
      "Reply-to: drosen@sensenetworks.com\nBCC: drosen@sensenetworks.com")

;; add Cc and Bcc headers to the message buffer (for the message-user-agent -- different customization than for mail, above)
(setq message-default-mail-headers "CC: \nBCC: drosen@sensenetworks.com\nReply-to: drosen@sensenetworks.com\n")

;; Unfill paragraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;; Handy key definition
;;(define-key global-map "\M-Q" 'unfill-paragraph)

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; Handy key definition
(define-key global-map "\C-\M-q" 'unfill-region)

;; HTML Stuff
(add-hook 'html-mode-hook
	  (lambda ()
	    ;; Default indentation is usually 2 spaces, changing to 4.
	    (set (make-local-variable 'sgml-basic-offset) 4)))

;; I do this so that emacs uses the path from my shell, and thus will
;; hopefully use the right python
(defun set-exec-path-from-shell-PATH ()
        (interactive)
        (let ((path-from-shell (replace-regexp-in-string "^.*\n.*shell\n" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;;(setq python-python-command "~/your/python/bin-dir/python")


;;
;; ace jump mode major function
;;
;;(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)



;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

