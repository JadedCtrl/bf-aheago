;; —————————————————————————————————————
;; PACKAGE

(defpackage :bf-aheago
  (:use :cl :anaphora)
  (:export :interpret)
  (:nicknames :bf-a))

(in-package :bf-aheago)

;; —————————————————————————————————————
;; MACROS

(defmacro interpret-char (char)
  `(progn 
     (when debug-p (format *error-output* "~A" ,char))
     (cond
       ((eq ,char #\.) (output-cell tape pointer))
       ((eq ,char #\,) (input-cell tape pointer))
       ((eq ,char #\<) (bound-decf pointer tape-length))
       ((eq ,char #\>) (bound-incf pointer tape-length))
       ((eq ,char #\+) (inc-cell tape pointer))
       ((eq ,char #\-) (dec-cell tape pointer))
       ((eq ,char #\[) (loop-advance tape pointer input-stream))
       ((eq ,char #\]) (loop-rewind tape pointer input-stream)))))

;; VARYING [NUMBER] [NUMBER] → NUMBER
(defmacro bound-incf (object &optional (max 256) (min 0))
  "Increment (destructive) an object, but  bounds-check with #'bound-ensure."
  `(setf ,object (bound-ensure (1+ ,object) ,max ,min)))

;; VARYING [NUMBER] [NUMBER] → NUMBER
(defmacro bound-decf (object &optional (max 256) (min 0))
  "Decrement (destructive) an object, but  bounds-check with #'bound-ensure."
  `(setf ,object (bound-ensure (1- ,object) ,max ,min)))

;; —————————————————————————————————————
;; INTERPRETER

;; STREAM → ARRAY
(defmethod interpret ((input-stream stream) &key (tape-length 30000) (debug-p nil))
  "Interpret the brainfuck code within the given stream: returns the tape."
  (let ((tape (make-tape tape-length))
        (pointer 0))
    (loop :if (not (listen input-stream))
          :return tape
          :do (alet (read-char input-stream)
                (interpret-char it)))))

(defmethod interpret ((string string) &key (tape-length 30000) (debug-p nil))
  (interpret (make-input-string string) :tape-length tape-length :debug-p debug-p))

(defmethod interpret ((pathname pathname) &key (tape-length 30000) (debug-p nil))
  (with-open-file (stream pathname)
    (interpret stream :tape-length tape-length :debug-p debug-p)))

;; —————————————————————————————————————
;; CELLS

;; ARRAY NUMBER → NUMBER
(defun inc-cell (tape index)
  "Increment the given cell."
  (bound-incf (aref tape index)))
  ; (setf (aref tape index) (bound-ensure (1+ (aref tape index)))))

;; ARRAY NUMBER → NUMBENR
(defun dec-cell (tape index)
  "Decrement the given cell."
  (bound-decf (aref tape index)))
  ; (setf (aref tape index) (bound-ensure (1- (aref tape index)))))

;; ARRAY NUMBER → NIL
(defun output-cell (tape index)
  "Print the given cell in the tape to stdout."
  (format t "~A" (code-char (aref tape index))))

;; ARRAY NUMBER → CHAR
(defun input-cell (tape index)
  "Input a char's int into the tape at given index."
  (alet (read-char *standard-input* nil 0)
    (setf (aref tape index) (if (numberp it) it (char-code it)))))

;; [NUMBER] → ARRAY
(defun make-tape (&optional (length 30000))
  "Make a clean, 0-initialized BF tape."
  (make-array (list length) :initial-element 0))

;; —————————————————————————————————————
;; LOOPING []

;; ARRAY NUMBER STREAM → NIL
(defun loop-rewind (tape index stream)
  "Restart the loop (move pointer to last '[') if nonzero cell value."
  (if (not (zerop (aref tape index)))
    (stream-rewind-to stream #\[)))

;; ARRAY NUMBER STREAM → NIL
(defun loop-advance (tape index stream)
  "Skip the loop (move to next ']') if cell value is zero."
  (if (zerop (aref tape index))
    (stream-advance-to stream #\])))

;; —————————————————————————————————————
;; STREAM MANIP

;; STREAM → CHAR
(defun retroread-char (stream)
  "Read the previous character in a file-stream."
  (alet (file-position stream)
    (file-position stream (- it 2)))
  (read-char stream))

;; STREAM CHAR → NIL
(defun stream-advance-to (stream char)
  "Advance a stream's pointer until the given character is read."
  (if (not (eq char (read-char stream)))
    (stream-advance-to stream char)))

;; STREAM CHAR → NIL
(defun stream-rewind-to (stream char)
  "Reverse a stream's pointer until the given character is read."
  (if (not (eq char (retroread-char stream)))
    (stream-rewind-to stream char)))

;; —————————————————————————————————————
;; MISC

;; NUMBER [NUMBER] [NUMBER]
(defun bound-ensure (number &optional (max 256) (min 0))
  "Ensure the given number remains within the given bounds (with overflow)."
  (cond ((> min number) (bound-ensure (+ number max) max min))
        ((< max number) (bound-ensure (- max number) max min))
        (T number)))
