;; ~/.emacs should be a symbolic link to this file
;; ~/emacs should be a symbolic link to the containing directory

(load-file "~/emacs/init-packages.el")
(load-file "~/emacs/system-specific.el")

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

;; RECENT FILES
(require 'recentf)			; Add recent opened files menu
(recentf-mode 1)

;; DRAG & DROP FILES INTO EMACS
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

;; SHELL tweaks (comint) 
;; other Shell stuff from http://snarfed.org/space/why_I_dont_run_shells_inside_Emacs
(require 'comint)
(define-key comint-mode-map [(meta p)]
   'comint-previous-matching-input-from-input)
(define-key comint-mode-map [(meta n)]
   'comint-next-matching-input-from-input)
(define-key comint-mode-map [(control meta n)]
    'comint-next-input)
(define-key comint-mode-map [(control meta p)]
    'comint-previous-input)
; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

;; MISC STUFF
(setq case-fold-search t) ; make searches case insensitive
(transient-mark-mode 1)		 ; Make regions (aka blocks) highlight
(setq inhibit-startup-message t) ; Don't show startup screen

;; Automatically decompress when opening gzipped files
(auto-compression-mode  1)

;; UNFILL PARAGRAPH
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))
;; Handy key definition
(define-key global-map "\C-\M-q" 'unfill-region)
