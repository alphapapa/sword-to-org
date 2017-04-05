(defconst sword-to-org--diatheke-parse-regexp
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

(defun sword-to-org--diatheke-parse-text (text)
  "Return parsed plist from TEXT."
  (string-match sword-to-org--diatheke-parse-regexp text)
  (let ((book (match-string 1 text))
        (chapter (string-to-number (match-string 2 text)))
        (verse (string-to-number (match-string 3 text)))
        (text (s-trim (match-string 4 text))))
    (list :book book :chapter chapter :verse verse :text text)))


;; (sword-to-org--diatheke-get-text "ESV" "gen 1:1")

;; (sword-to-org--diatheke-parse-text (sword-to-org--diatheke-get-text "ESV" "gen 1:1"))

;; (cl-loop for verse-number in '(1 2 3)
;;          collect (sword-to-org--diatheke-parse-text (sword-to-org--diatheke-get-text "ESV" (number-to-string verse-number))))
