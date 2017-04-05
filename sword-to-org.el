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
