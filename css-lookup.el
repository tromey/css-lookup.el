;;; css-lookup.el --- Like info-lookup-symbol, for CSS -*-lexical-binding:t-*-

;; Author: Tom Tromey <tom@tromey.com>
;; Version: 1.0
;; URL: https://github.com/tromey/css-lookup.el
;; Keywords: help languages

;;; Code:

(require 'css-mode)
(require 'eww)

(defvar css--lookup-history nil)

(defconst css--mdn-url "https://developer.mozilla.org/en-US/docs/Web/CSS/")
(defconst css--mdn-xhr-params "?raw&macros")

(defun css--after-render ()
  (setf header-line-format nil)
  (goto-char (point-min))
  (when (re-search-forward "^Summary" nil 'move)
    (beginning-of-line)))

(defconst css--mdn-symbol-regexp
  "Regular expression to match the CSS symbol at point."
  (concat "\\(?:\\(@" (regexp-opt css-at-ids)
	  "\\)\\|\\(\\b" (regexp-opt css-property-ids)
	  "\\)\\s-*\\(?:\\=\\|:\\)\\)"))

(defconst css--mdn-completion-list
  "List of all symbols available for lookup via MDN."
  (nconc (mapcar (lambda (atrule) (concat "@" atrule)) css-at-ids)
	 css-property-ids))

(defun css--find-symbol ()
  (save-excursion
    (skip-chars-backward "-[:alnum:]")
    (if (looking-at css--mdn-symbol-regexp)
	(or (match-string-no-properties 1)
	    (match-string-no-properties 2))
      (let ((bound (save-excursion
		     (beginning-of-line)
		     (point))))
	(when (re-search-backward css--mdn-symbol-regexp bound t)
	  (or (match-string-no-properties 1)
	      (match-string-no-properties 2)))))))

;;;###autoload
(defun css-lookup-symbol (symbol)
  "Display the CSS documentation for SYMBOL, as found on MDN.
When this command is used interactively, it picks a default
symbol based on the CSS text before point -- either an @-keyword
or a property name, depending on what is seen."
  (interactive
   (list
    (let* ((sym (css--find-symbol))
	   (enable-recursive-minibuffers t)
	   (value (completing-read
		   (if sym
		       (format "Describe CSS symbol (default %s): " sym)
		     "Describe CSS symbol: ")
		   css--mdn-completion-list nil nil nil
		   'css--lookup-history sym)))
      (if (equal value "") sym value))))
  (when symbol
    (let ((url (concat css--mdn-url symbol css--mdn-xhr-params)))
      (with-current-buffer (get-buffer-create "*MDN CSS*")
	(eww-mode)
	;; Make sure to display the buffer before calling `eww', as
	;; that calls `pop-to-buffer-same-window'.
	(display-buffer (current-buffer) t)
	(add-hook 'eww-after-render-hook #'css--after-render nil t)
	(eww url)))))

;;;###autoload(eval-after-load 'css-mode '(define-key css-mode-map [remap info-lookup-symbol] 'css-lookup-symbol))

;;; css-lookup.el ends here
