
;;; TEST FUNCTION

(defun run-tests ()
  (let ((key #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))

    ;; Тест 1: Пустое сообщение (0 байт)
    (let* ((data #())
           (enc (encrypt-data data key))
           (dec (decrypt-data enc key)))
      (print "========================")
      (print "Test 1: empty message")
      (print "Original:") (print data)
      (print "Encrypted:") (print enc)
      (print "Decrypted:") (print dec)
      (print "OK?")
      (print (equalp data dec)))

    ;; Тест 2: Сообщение длиной 1 байт
    (let* ((data #(255))
           (enc (encrypt-data data key))
           (dec (decrypt-data enc key)))
      (print "========================")
      (print "Test 2: single byte")
      (print "Original:") (print data)
      (print "Encrypted:") (print enc)
      (print "Decrypted:") (print dec)
      (print "OK?")
      (print (equalp data dec)))

    ;; Тест 3: Короткое сообщение (2 байта)
    (let* ((data #(10 20))
           (enc (encrypt-data data key))
           (dec (decrypt-data enc key)))
      (print "========================")
      (print "Test 3: short data")
      (print "Original:") (print data)
      (print "Encrypted:") (print enc)
      (print "Decrypted:") (print dec)
      (print "OK?")
      (print (equalp data dec)))

    ;; Тест 4: Сообщение размером ровно в блок (16 байт)
    (let* ((data #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
           (enc (encrypt-data data key))
           (dec (decrypt-data enc key)))
      (print "========================")
      (print "Test 4: one block")
      (print "Original:") (print data)
      (print "Encrypted:") (print enc)
      (print "Decrypted:") (print dec)
      (print "OK?")
      (print (equalp data dec)))

    ;; Тест 5: Длинное сообщение (4 блока, 64 байта)
    (let* ((data #(10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160
                   1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                   255 254 253 252 251 250 249 248 247 246 245 244 243 242
                   241 240 100 101 102 103 104 105 106 107 108 109 110 111
                   112 113 114 115))
           (enc (encrypt-data data key))
           (dec (decrypt-data enc key)))
      (print "========================")
      (print "Test 5: long data")
      (print "Original:") (print data)
      (print "Encrypted:") (print enc)
      (print "Decrypted:") (print dec)
      (print "OK?")
      (print (equalp data dec)))))

(run-tests)