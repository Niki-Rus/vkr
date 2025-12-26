;;; AES CONSTANTS

(defparameter *s-box*
  #(99 124 119 123 242 107 111 197 48 1 103 43 254 215 171 118
    202 130 201 125 250 89 71 240 173 212 162 175 156 164 114 192
    183 253 147 38 54 63 247 204 52 165 229 241 113 216 49 21
    4 199 35 195 24 150 5 154 7 18 128 226 235 39 178 117
    9 131 44 26 27 110 90 160 82 59 214 179 41 227 47
    132 83 209 0 237 32 252 177 91 106 203 190 57 74
    76 88 207 208 239 170 251 67 77 51 133 69 249 2
    127 80 60 159 168 81 163 64 143 146 157 56 245 188
    182 218 33 16 255 243 210 205 12 19 236 95 151 68
    23 196 167 126 61 100 93 25 115 96 129 79 220 34
    42 144 136 70 238 184 20 222 94 11 219 224 50 58
    10 73 6 36 92 194 211 172 98 145 149 228 121 231
    200 55 109 141 213 78 169 108 86 244 234 101 122 174
    8 186 120 37 46 28 166 180 198 232 221 116 31 75 189
    139 138 112 62 181 102 72 3 246 14 97 53 87 185
    134 193 29 158 225 248 152 17 105 217 142 148 155 30
    135 233 206 85 40 223 140 161 137 13 191 230 66 104
    65 153 45 15 176 84 187 22))

(defparameter *inv-s-box*
  (let ((a (make-array 256)))
    (dotimes (i 256 a)
      (setf (aref a (aref *s-box* i)) i))))

(defparameter *rcon* #(1 2 4 8 16 32 64 128 27 54))

(defun xor-vectors (a b)
  (let ((r (make-array 16)))
    (dotimes (i 16 r)
      (setf (aref r i) (logxor (aref a i) (aref b i))))))

(defun sub-bytes (s)
  (let ((r (make-array 16)))
    (dotimes (i 16 r)
      (setf (aref r i) (aref *s-box* (aref s i))))))

(defun inv-sub-bytes (s)
  (let ((r (make-array 16)))
    (dotimes (i 16 r)
      (setf (aref r i) (aref *inv-s-box* (aref s i))))))

(defun shift-rows (s)
  (vector
   (aref s 0) (aref s 5) (aref s 10) (aref s 15)
   (aref s 4) (aref s 9) (aref s 14) (aref s 3)
   (aref s 8) (aref s 13) (aref s 2) (aref s 7)
   (aref s 12) (aref s 1) (aref s 6) (aref s 11)))

(defun inv-shift-rows (s)
  (vector
   (aref s 0) (aref s 13) (aref s 10) (aref s 7)
   (aref s 4) (aref s 1) (aref s 14) (aref s 11)
   (aref s 8) (aref s 5) (aref s 2) (aref s 15)
   (aref s 12) (aref s 9) (aref s 6) (aref s 3)))

(defun xtime (x)
  (logand (if (>= x 128)
              (logxor (ash x 1) #x1b)
              (ash x 1))
          255))

(defun mix-columns (s)
  (let ((r (make-array 16)))
    (dotimes (c 4 r)
      (let* ((i (* c 4))
             (a (aref s i))
             (b (aref s (+ i 1)))
             (c1 (aref s (+ i 2)))
             (d (aref s (+ i 3))))
        (setf (aref r i)       (logxor (xtime a) (logxor (xtime b) b) c1 d))
        (setf (aref r (+ i 1)) (logxor a (xtime b) (logxor (xtime c1) c1) d))
        (setf (aref r (+ i 2)) (logxor a b (xtime c1) (logxor (xtime d) d)))
        (setf (aref r (+ i 3)) (logxor (logxor (xtime a) a) b c1 (xtime d)))))))

(defun inv-mix-columns (s)
  (labels ((mul (x n)
             (cond ((= n 9)  (logxor (xtime (xtime (xtime x))) x))
                   ((= n 11) (logxor (xtime (xtime (xtime x))) (xtime x) x))
                   ((= n 13) (logxor (xtime (xtime (xtime x))) (xtime (xtime x)) x))
                   ((= n 14) (logxor (xtime (xtime (xtime x)))
                                     (xtime (xtime x))
                                     (xtime x))))))
    (let ((r (make-array 16)))
      (dotimes (c 4 r)
        (let* ((i (* c 4))
               (a (aref s i))
               (b (aref s (+ i 1)))
               (c1 (aref s (+ i 2)))
               (d (aref s (+ i 3))))
          (setf (aref r i)       (logxor (mul a 14) (mul b 11) (mul c1 13) (mul d 9)))
          (setf (aref r (+ i 1)) (logxor (mul a 9) (mul b 14) (mul c1 11) (mul d 13)))
          (setf (aref r (+ i 2)) (logxor (mul a 13) (mul b 9) (mul c1 14) (mul d 11)))
          (setf (aref r (+ i 3)) (logxor (mul a 11) (mul b 13) (mul c1 9) (mul d 14))))))))

;;; PKCS#7

(defun pkcs7-pad (data)
  (let* ((len (length data))
         (pad (- 16 (mod len 16)))
         (r (make-array (+ len pad))))
    (dotimes (i len)
      (setf (aref r i) (aref data i)))
    (dotimes (i pad)
      (setf (aref r (+ len i)) pad))
    r))

(defun pkcs7-unpad (data)
  (let ((pad (aref data (1- (length data)))))
    (subseq data 0 (- (length data) pad))))
