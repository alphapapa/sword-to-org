(defconst sword-to-org--diatheke-parse-line-regexp
  (rx bol
      ;; Book name
      (group-n 1 (minimal-match (1+ anything)))
      space
      ;; chapter:verse
      (group-n 2 (1+ digit)) ":" (group-n 3 (1+ digit)) ":"
      space
      (group-n 4 (1+ anything))))

(defun sword-to-org--diatheke-get-text (module key)
  "Get text from diatheke MODULE for KEY."
  (with-temp-buffer
    (call-process "diatheke" nil '(t nil) nil
                  "-b" module "-k" key)
    (buffer-substring (point-min) (save-excursion
                                    (goto-char (point-max))
                                    (forward-line -2)
                                    (end-of-line)
                                    (point)))))

(defun sword-to-org--diatheke-parse-text (text &optional &key keep-newlines)
  "Parse TEXT line-by-line, returning list of verse plists.
When KEEP-NEWLINES is non-nil, keep blank lines in text."
  (cl-loop with result
           with new-verse
           for line in (s-lines text)
           for parsed = (sword-to-org--diatheke-parse-line line)
           if parsed
           do (progn
                (when new-verse
                  (push new-verse result))
                (setq new-verse parsed))
           else do (let* ((text (plist-get new-verse :text))
                          (new-text (concat text
                                            (if (s-present? line)
                                                line
                                              (when keep-newlines "\n")))))
                     (plist-put new-verse :text new-text))
           finally return (progn
                            (push new-verse result)
                            (nreverse result))))

(defun sword-to-org--diatheke-parse-line (line)
  "Return parsed plist from LINE."
  (if (s-present? line)
      (when (string-match sword-to-org--diatheke-parse-line-regexp line)
        (let ((book (match-string 1 line))
              (chapter (string-to-number (match-string 2 line)))
              (verse (string-to-number (match-string 3 line)))
              (text (s-trim (match-string 4 line))))
          (list :book book :chapter chapter :verse verse :text text)))))


;; (sword-to-org--diatheke-get-text "ESV" "gen 1:1")

;; (sword-to-org--diatheke-parse-line (sword-to-org--diatheke-get-text "ESV" "gen 1:1"))

;; (cl-loop for verse-number in '(1 2 3) append (sword-to-org--diatheke-parse-text (sword-to-org--diatheke-get-text "ESV" (number-to-string verse-number))))

;; (sword-to-org--diatheke-parse-text (sword-to-org--diatheke-get-text "ESV" "Philemon 1:1-3") :keep-newlines t)
