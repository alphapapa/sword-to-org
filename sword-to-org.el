;;; sword-to-org.el --- Convert Sword modules to Org outlines

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/sword-to-org
;; Version: 0.0.1-pre
;; Package-Requires: ((emacs "24.4") (dash "2.11") (s "1.10.0"))
;; Keywords: outlines, org-mode, sword, research, bible

;;; Commentary:

;; This package uses the `diatheke' program to convert Sword modules
;; to Org-mode outlines.  For example, you can make an Org file
;; containing the entire text of the ESV module as an outline
;; structured by book/chapter/verse.  Then you can add top-level
;; headings for Old/New Testaments, and then you have the whole Bible
;; as an Org file.  Then you can do everything you can do in Org with
;; the text of the Bible!  Add footnotes, links, tags, properties,
;; write your own commentaries under subheadings, organize research
;; with TODO items, export with `org-export', search with
;; `helm-org-rifle', etc.  The list is endless.

;;; Usage:

;; First install `diatheke'.  On Debian/Ubuntu it's in the `diatheke'
;; package.

;; Open a buffer and run the command `sword-to-org-insert-outline'.
;; Choose the module (e.g. Bible translation) to use, then input a
;; passage reference or range (e.g. "Gen 1", "Jn 1:1", or even
;; "Gen-Rev"--that last one will take a few moments), and an Org
;; outline will be inserted in book/chapter/verse/text structure.

;; You may customize `sword-to-org-default-module' so you don't have
;; to pick a module every time, and you can call the command with a
;; universal prefix (`C-u') to choose a different module.

;; You may also use any of the `sword-to-org--' support functions in
;; your own programs.  Consult the docstrings for instructions and
;; examples.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'dash)
(require 's)

;;;; Variables

(defconst sword-to-org--diatheke-parse-line-regexp
  (rx bol
      ;; Book name
      (group-n 1 (minimal-match (1+ anything)))
      space
      ;; chapter:verse
      (group-n 2 (1+ digit)) ":" (group-n 3 (1+ digit)) ":"
      ;; Passage text (which may start with a newline, in which case
      ;; no text will be on the same line after chapter:verse)
      (optional (1+ space)
                (group-n 4 (1+ anything))))
  "Regexp to parse each line of output from `diatheke'.")

(defgroup sword-to-org nil
  "Settings for `sword-to-org'."
  :link '(url-link "http://github.com/alphapapa/sword-to-org")
  :group 'org)

(defcustom sword-to-org-default-module nil
  "Default module (e.g. Bible translation, like \"ESV\") to use."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Module abbreviation (e.g. \"ESV\")")))

;;;; Functions

;;;;; Commands

;;;###autoload
(defun sword-to-org-insert-outline (module key)
  "Insert Org outline in current buffer for Sword MODULE and KEY.
The buffer will be switched to `text-mode' before inserting, to
improve performance, and then switched back to `org-mode' if
it was active."
  (interactive (list (if (or current-prefix-arg
                             (not sword-to-org-default-module))
                         (completing-read "Module: " (sword-to-org--diatheke-get-modules))
                       sword-to-org-default-module)
                     (read-from-minibuffer "Passage: ")))
  (let ((was-org-mode (eq major-mode 'org-mode)))
    (when was-org-mode
      (text-mode))
    (cl-loop with last-book
             with last-chapter
             for passage in (sword-to-org--diatheke-parse-text
                             (sword-to-org--diatheke-get-text module key))
             do (-let (((&plist :book book :chapter chapter :verse verse :text text) passage))
                  (unless (equal book last-book)
                    (insert (format "** %s\n\n" book))
                    (setq last-chapter nil)
                    (setq last-book book))
                  (unless (equal chapter last-chapter)
                    (insert (format "*** %s %s\n\n" book chapter))
                    (setq last-chapter chapter))
                  (insert (format "**** %s %s:%s\n\n%s\n\n" book chapter verse text))))
    (when was-org-mode
      (org-mode))))

;;;###autoload
(defun sword-to-org-insert-passage (key &optional separate-lines module)
  "Insert passage for reference KEY as plain text.
With prefix, prompt for module, otherwise use default module.
With double-prefix, insert each verse on its own line with
reference; otherwise, insert as single paragraph with reference
at the end."
  (interactive (list (read-from-minibuffer "Passage: ")
                     (equal current-prefix-arg '(16))
                     (if (or current-prefix-arg
                             (not sword-to-org-default-module))
                         (completing-read "Module: " (sword-to-org--diatheke-get-modules))
                       sword-to-org-default-module)))
  (insert (sword-to-org--passage key :module module :paragraph (not separate-lines)))
  (when (not separate-lines)
    (insert " (" key ")")))

;;;;; Support

(cl-defun sword-to-org--passage (key &key module paragraph)
  "Return string for passage reference KEY.
If MODULE is nil, use default module.  If PARAGRAPH is non-nil,
join all verses into a paragraph; otherwise put each verse on its
own line with reference."
  (unless module
    (setq module sword-to-org-default-module))
  (if paragraph
      (s-join " " (cl-loop for passage in (sword-to-org--diatheke-parse-text (sword-to-org--diatheke-get-text module key))
                           collect (plist-get passage :text)))
    ;; NOTE: Using double-newline as verse separator so the verses
    ;; can appear separately in Org exports from Org Babel blocks
    ;; (for some reason, single newlines are replaced with spaces)
    (s-join "\n\n" (cl-loop for passage in (sword-to-org--diatheke-parse-text (sword-to-org--diatheke-get-text module key))
                            collect (-let (((&plist :book book :chapter chapter :verse verse :text text) passage))
                                      (format "%s %s:%s  %s" book chapter verse text))))))

(defun sword-to-org--diatheke-get-modules ()
  "Return list of Sword modules from diatheke.
Only the module abbreviation is returned."
  (cl-loop for line in (s-lines (with-temp-buffer
                                  (call-process "diatheke" nil '(t nil) nil
                                                "-b" "system" "-k" "modulelist")
                                  (buffer-string)))
           when (string-match (rx (group-n 1 (minimal-match (1+ (not (any ":"))))) " : ") line)
           collect (match-string 1 line)))

(defun sword-to-org--diatheke-get-text (module key)
  "Return raw text from diatheke MODULE for KEY.
This simply calls `diatheke -b MODULE -k KEY' and returns the raw output.

Examples:

\(sword-to-org--diatheke-get-text \"ESV\" \"gen 1:1\")"
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
When KEEP-NEWLINES is non-nil, keep blank lines in text.

Plists are in format (:book \"Genesis\" :chapter 1 :verse 1
                      :text \"In the beginning...\").

Example:

\(sword-to-org--diatheke-parse-text
  (sword-to-org--diatheke-get-text \"ESV\" \"Philemon 1:1-3\")
  :keep-newlines t)"
  (cl-loop with result
           with new-verse
           for line in (s-lines text)
           for parsed = (sword-to-org--diatheke-parse-line line)
           if parsed
           do (progn
                (push new-verse result)
                (setq new-verse parsed))
           else do (let* ((text (plist-get new-verse :text))
                          (new-text (concat text
                                            (if (s-present? line)
                                                line
                                              (when keep-newlines "\n")))))
                     (plist-put new-verse :text new-text))
           finally return (cdr (progn
                                 (push new-verse result)
                                 (nreverse result)))))

(defun sword-to-org--diatheke-parse-line (line)
  "Return plist from LINE.  If LINE is not the beginning of a verse, return nil.
You generally don't want to use this directly.  Instead use
`sword-to-org--diatheke-parse-text'.

Plist is in format (:book \"Genesis\" :chapter 1 :verse 1
                    :text \"In the beginning...\").

For a complete example, see how
`sword-to-org--diatheke-parse-text' calls this function."
  (if (s-present? line)
      (when (string-match sword-to-org--diatheke-parse-line-regexp line)
        (let ((book (match-string 1 line))
              (chapter (string-to-number (match-string 2 line)))
              (verse (string-to-number (match-string 3 line)))
              ;; Ensure text is present, which may not be the case if
              ;; a verse starts with a newline.  See
              ;; <https://github.com/alphapapa/sword-to-org/issues/2>
              (text (when (s-present? (match-string 4 line))
                      (s-trim (match-string 4 line)))))
          (list :book book :chapter chapter :verse verse :text text)))))

(provide 'sword-to-org)

;;; sword-to-org.el ends here
