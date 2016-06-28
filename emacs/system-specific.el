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
      (setq send-mail-function (quote mailclient-send-it))
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)
      ;; Open files, each in a new buffer, when drag and dropping onto emacs
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
	)
      )
  )

