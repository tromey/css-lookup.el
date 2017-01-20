;;; css-lookup.el --- Like info-lookup-symbol, for CSS -*-lexical-binding:t-*-

;; Author: Tom Tromey <tom@tromey.com>
;; Version: 1.2
;; URL: https://github.com/tromey/css-lookup.el
;; Keywords: help languages

;;; Code:

(require 'css-mode)
(require 'eww)

(defvar css--mdn-lookup-history nil)

(defcustom css-lookup-url-format
  "https://developer.mozilla.org/en-US/docs/Web/CSS/%s?raw&macros"
  "Format for a URL where CSS documentation can be found.
The format should include a single \"%s\" substitution.
The name of the CSS property, @-id, pseudo-class, or pseudo-element
to look up will be substituted there."
  :version "26.1"
  :type 'string
  :group 'css)

(defun css--mdn-after-render ()
  (setf header-line-format nil)
  (goto-char (point-min))
  (let ((window (get-buffer-window (current-buffer) 'visible)))
    (when window
      (when (re-search-forward "^Summary" nil 'move)
  	(beginning-of-line)
  	(set-window-start window (point))))))

(defconst css--mdn-symbol-regexp
  (concat "\\("
	  ;; @-ids.
	  "\\(@" (regexp-opt css-at-ids) "\\)"
	  "\\|"
	  ;; ;; Known properties.
	  (regexp-opt css-property-ids t)
	  "\\|"
	  ;; Pseudo-classes.
	  "\\(:" (regexp-opt css-pseudo-class-ids) "\\)"
	  "\\|"
	  ;; Pseudo-elements with either one or two ":"s.
	  "\\(::?" (regexp-opt css-pseudo-element-ids) "\\)"
	  "\\)")
  "Regular expression to match the CSS symbol at point.")

(defconst css--mdn-property-regexp
  (concat "\\_<" (regexp-opt css-property-ids t) "\\s-*\\(?:\\=\\|:\\)")
  "Regular expression to match a CSS property.")

(defconst css--mdn-completion-list
  (nconc
   ;; @-ids.
   (mapcar (lambda (atrule) (concat "@" atrule)) css-at-ids)
   ;; Pseudo-classes.
   (mapcar (lambda (class) (concat ":" class)) css-pseudo-class-ids)
   ;; Pseudo-elements with either one or two ":"s.
   (mapcar (lambda (elt) (concat ":" elt)) css-pseudo-element-ids)
   (mapcar (lambda (elt) (concat "::" elt)) css-pseudo-element-ids)
   ;; Properties.
   css-property-ids)
  "List of all symbols available for lookup via MDN.")

(defun css--mdn-find-symbol ()
  "A helper for `css-lookup-symbol' that finds the symbol at point.
Returns the symbol, a string, or `nil' if none found."
  (save-excursion
    ;; Skip backward over a word first.
    (skip-chars-backward "-[:alnum:]")
    ;; Now skip ":" or "@" to see if it's a pseudo-element or at-id.
    (skip-chars-backward "@:")
    (if (looking-at css--mdn-symbol-regexp)
	(match-string-no-properties 0)
      (let ((bound (save-excursion
		     (beginning-of-line)
		     (point))))
	(when (re-search-backward css--mdn-property-regexp bound t)
	  (match-string-no-properties 1))))))

;;;###autoload
(defun css-lookup-symbol (symbol)
  "Display the CSS documentation for SYMBOL, as found on MDN.
When this command is used interactively, it picks a default
symbol based on the CSS text before point -- either an @-keyword,
a property name, a pseudo-class, or a pseudo-element, depending
on what is seen near point."
  (interactive
   (list
    (let* ((sym (css--mdn-find-symbol))
	   (enable-recursive-minibuffers t)
	   (value (completing-read
		   (if sym
		       (format "Describe CSS symbol (default %s): " sym)
		     "Describe CSS symbol: ")
		   css--mdn-completion-list nil nil nil
		   'css--mdn-lookup-history sym)))
      (if (equal value "") sym value))))
  (when symbol
    ;; If we see a single-colon pseudo-element like ":after", turn it
    ;; into "::after".
    (when (and (eq (aref symbol 0) ?:)
	       (member (substring symbol 1) css-pseudo-element-ids))
      (setq symbol (concat ":" symbol)))
    (let ((url (format css-lookup-url-format symbol)))
      (with-current-buffer (get-buffer-create "*MDN CSS*")
	(eww-mode)
	;; Make sure to display the buffer before calling `eww', as
	;; that calls `pop-to-buffer-same-window'.
	(display-buffer (current-buffer) t)
	(add-hook 'eww-after-render-hook #'css--mdn-after-render nil t)
	(eww url)))))

;;;###autoload(eval-after-load 'css-mode '(define-key css-mode-map [remap info-lookup-symbol] 'css-lookup-symbol))

;;; css-lookup.el ends here
