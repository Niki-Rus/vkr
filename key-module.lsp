
;;; KEY MODULE 

(defun rot-word (w)
  (logior (ash (logand w #xFFFFFF) 8)
          (ldb (byte 8 24) w)))

(defun sub-word (w)
  (logior (ash (aref *s-box* (ldb (byte 8 24) w)) 24)
          (ash (aref *s-box* (ldb (byte 8 16) w)) 16)
          (ash (aref *s-box* (ldb (byte 8 8) w)) 8)
          (aref *s-box* (ldb (byte 8 0) w))))

(defun key-expansion (key)
  (let ((w (make-array 44)))
    (dotimes (i 4)
      (setf (aref w i)
            (logior (ash (aref key (* i 4)) 24)
                    (ash (aref key (+ (* i 4) 1)) 16)
                    (ash (aref key (+ (* i 4) 2)) 8)
                    (aref key (+ (* i 4) 3)))))
    (dotimes (i 40)
      (let ((tmp (aref w (+ i 3))))
        (when (= (mod (+ i 4) 4) 0)
          (setf tmp (logxor (sub-word (rot-word tmp))
                            (ash (aref *rcon*
                                       (1- (/ (+ i 4) 4))) 24))))
        (setf (aref w (+ i 4)) (logxor (aref w i) tmp))))
    w))

(defun round-key (w r)
  (let ((k (make-array 16)))
    (dotimes (i 4 k)
      (let ((v (aref w (+ (* r 4) i))))
        (setf (aref k (* i 4))     (ldb (byte 8 24) v))
        (setf (aref k (+ (* i 4) 1)) (ldb (byte 8 16) v))
        (setf (aref k (+ (* i 4) 2)) (ldb (byte 8 8) v))
        (setf (aref k (+ (* i 4) 3)) (ldb (byte 8 0) v))))))

