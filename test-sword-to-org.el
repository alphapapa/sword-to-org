(require 'buttercup)

(describe "sword-to-org"
  (it "Can get Genesis 1:1-3 from the ESV with Diatheke"
    (expect (cl-loop for verse-number in '(1 2 3)
                     append (sword-to-org--diatheke-parse-text
                             (sword-to-org--diatheke-get-text "ESV" (number-to-string verse-number))))

            :to-equal '((:book "Genesis" :chapter 1 :verse 1 :text "In the beginning, God created the heavens and the earth.")
                        (:book "Genesis" :chapter 1 :verse 2 :text "The earth was without form and void, and darkness was over the face of the deep. And the Spirit of God was hovering over the face of the waters.")
                        (:book "Genesis" :chapter 1 :verse 3 :text "And God said, Let there be light, and there was light."))))

  (it "Can get Philemon 1-3 from the ESV with Diatheke"
    (expect (sword-to-org--diatheke-parse-text
             (sword-to-org--diatheke-get-text "ESV" "Philemon 1:1-3")
             :keep-newlines t)

            :to-equal '((:book "Philemon" :chapter 1 :verse 1 :text "Paul, a prisoner for Christ Jesus, and Timothy our brother,
To Philemon our beloved fellow worker")
                        (:book "Philemon" :chapter 1 :verse 2 :text "and Apphia our sister and Archippus our fellow soldier, and the church in your house:

")
                        (:book "Philemon" :chapter 1 :verse 3 :text "Grace to you and peace from God our Father and the Lord Jesus Christ.

")))))
