
;;; ENCRYPTION

(defun encrypt-block (data key)
  (let* ((w (key-expansion key))
         (state (xor-vectors data (round-key w 0))))
    (dotimes (r 9)
      (setf state (sub-bytes state))
      (setf state (shift-rows state))
      (setf state (mix-columns state))
      (setf state (xor-vectors state (round-key w (1+ r)))))
    (setf state (sub-bytes state))
    (setf state (shift-rows state))
    (xor-vectors state (round-key w 10))))

(defun encrypt-data (data key)
  (let* ((p (pkcs7-pad data))
         (r (make-array (length p))))
    (dotimes (i (/ (length p) 16) r)
      (let ((b (encrypt-block
                (subseq p (* i 16) (+ (* i 16) 16))
                key)))
        (dotimes (j 16)
          (setf (aref r (+ (* i 16) j)) (aref b j)))))))
