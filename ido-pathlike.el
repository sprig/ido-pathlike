;;;;; -*- lexical-binding:t -*-
;;;;; ido-pathlike: Functions for completing from a list of path-like elements

(defun add-ido-temp-backspace-hook ()
  "Add a temporary (one-time) hook for ido, that detects deleting a '/' key"
  (let ((original-del-binding (key-binding (kbd "DEL"))))
    (local-set-key (kbd "DEL")
		   (lambda ()
		     (interactive)
		     (let ((pt (point)))
		       (if (string= "/" (buffer-substring (1- pt) pt))
			   (throw 'deleted-path-component nil)
			 (call-interactively original-del-binding))))))
  (remove-hook 'ido-minibuffer-setup-hook 'add-ido-temp-backspace-hook))


(defun ido-completing-path-like-read
    (prompt choices &optional predicate require-match initial-input hist def inherit-input-method)
    "Provide ido completion to a list of elements that are path-like (i.e. each element is of the form ([^/]*/)*[^/]*). Consult ido-completing-read for the argument specification."
    (message "Ido-completing path-like")
  (let ((done nil)
	(path (or initial-input ""))
	(new-path "")
	(clean-choices
	 (map 'cons
	  (lambda (choice) (cond
			    ((stringp choice) choice)
			    ((and (listp choice) (stringp (car choice)))
			     (car choice))
			    (t (error "Unrecognized structure"))
			    )
	    )
	  choices))
	(current-choices ()))
    (while (not done)
      (setq current-choices (filter-paths-beginning-component path
							      clean-choices))
      (let ((component
	     (catch 'deleted-path-component
	       (add-hook 'ido-minibuffer-setup-hook
			 'add-ido-temp-backspace-hook)
	       (ido-completing-read prompt current-choices nil t path)
	       )))
	(message path)
	(if component
	    (setq new-path component)
	  (setq new-path (remove-last-path-component path))
	  ))
      (cond
       ((or (string= new-path path)
	    (string-match "[^/]$" new-path)) (setq done t))
       (new-path (setq path new-path))
       (t (setq done t)
	  (setq new-path (or def "")))
       ))
    new-path
    )
  )

(provide 'ido-pathlike)
