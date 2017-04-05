(defun sword-to-org--diatheke-get-text (module key)
  "Get text from diatheke MODULE for KEY."
  (with-temp-buffer
    (call-process "diatheke" nil '(t nil) nil
                  "-b" module "-k" key)
    (buffer-string)))
