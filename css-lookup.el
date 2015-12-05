(require 'css-mode)
(require 'eww)

(defvar css--lookup-history nil)

(defconst css--mdn-url "https://developer.mozilla.org/en-US/docs/Web/CSS/")
(defconst css--mdn-xhr-params "?raw&macros")

(defun css--render (status window url buffer)
  (eww-render status url nil buffer)
  (with-current-buffer buffer
    (setf header-line-format nil)
    (goto-char (point-min))
    (when window
      (when (re-search-forward "^Summary" nil 'move)
	(beginning-of-line)
	(set-window-start window (point))))))

(defconst css--property-or-atrule-regexp
  (concat "\\(?:\\(@" (regexp-opt css-at-ids)
	  "\\)\\|\\(\\b" (regexp-opt css-property-ids)
	  "\\)\\s-*\\(?:\\=\\|:\\)\\)"))

(defconst css--properties-and-atrules
  (nconc (mapcar (lambda (atrule) (concat "@" atrule)) css-at-ids)
	 css-property-ids))

(defun css--find-symbol ()
  (save-excursion
    (skip-chars-backward "-[:alnum:]")
    (if (looking-at css--property-or-atrule-regexp)
	(or (match-string-no-properties 1)
	    (match-string-no-properties 2))
      (let ((bound (save-excursion
		     (beginning-of-line)
		     (point))))
	(when (re-search-backward css--property-or-atrule-regexp bound t)
	  (or (match-string-no-properties 1)
	      (match-string-no-properties 2)))))))

(defun css-lookup-symbol (symbol)
  (interactive
   (list
    (let* ((sym (css--find-symbol))
	   (enable-recursive-minibuffers t)
	   (value (completing-read
		   (if sym
		       (format "Describe CSS symbol (default %s): " sym)
		     "Describe CSS symbol: ")
		   css--properties-and-atrules nil nil nil
		   'css--lookup-history sym)))
      (if (equal value "") sym value))))
  (when symbol
    (let ((url (concat css--mdn-url symbol css--mdn-xhr-params)))
      (with-current-buffer (get-buffer-create "*MDN CSS*")
	(let ((inhibit-read-only t))
	  (remove-overlays)
	  (erase-buffer))
	(unless (eq major-mode 'eww-mode)
	  (eww-mode))
	(plist-put eww-data :url url)
	(plist-put eww-data :title "")
	(let ((inhibit-read-only t))
	  (insert (format "Loading %s..." url))
	  (goto-char (point-min)))
	(let ((window (display-buffer (current-buffer))))
	  (url-retrieve url #'css--render
			(list window url (current-buffer))))))))