
;;; DECRYPTION

(defun decrypt-block (data key)
  (let* ((w (key-expansion key))
         (state (xor-vectors data (round-key w 10))))
    (dotimes (r 9)
      (setf state (inv-shift-rows state))
      (setf state (inv-sub-bytes state))
      (setf state (xor-vectors state (round-key w (- 9 r))))
      (setf state (inv-mix-columns state)))
    (setf state (inv-shift-rows state))
    (setf state (inv-sub-bytes state))
    (xor-vectors state (round-key w 0))))

(defun decrypt-data (data key)
  (pkcs7-unpad
   (let ((r (make-array (length data))))
     (dotimes (i (/ (length data) 16) r)
       (let ((b (decrypt-block
                 (subseq data (* i 16) (+ (* i 16) 16))
                 key)))
         (dotimes (j 16)
           (setf (aref r (+ (* i 16) j)) (aref b j))))))))

