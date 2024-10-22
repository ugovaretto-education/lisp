;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
(require 'browse-url) ; part of gnu emacs
(defun my-lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
If there is a text selection (a phrase), use that.

This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    ;; external browser
    (browse-url (concat "http://en.wikipedia.org/wiki/" word))
    ;; in emacs:
    ;;(eww (concat "http://en.wikipedia.org/wiki/" word)) ; emacs's own browser
    ))
