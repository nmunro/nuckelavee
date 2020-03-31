(defpackage antioch
  (:use :cl))
(in-package :antioch)

;; Experimental language design
(defparameter *example-print* "(print \"Hello world!\")")
(defparameter *example-sum* "(+ 2 2 2)")
(defparameter *symbols* (make-hash-table))

(defun populate-symbols ()
  (setf (gethash :start-form *symbols*) #\()
  (setf (gethash :end-form *symbols*) #\))
  (setf (gethash :whitespace *symbols*) #\space)
  (setf (gethash :dbl-quote *symbols*) #\")
  (setf (gethash :single-quote *symbols*) #\')
  (setf (gethash :+ *symbols*) #\+))

(defclass TOKEN ()
  ((name :initarg :name :initform "" :reader name)
   (value :initarg :value :initform "" :reader value)))

(defmethod print-object ((obj TOKEN) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (format nil "[~A: ~A]" (name obj) (value obj)) stream)))

(defun make-operator (value)
  "Make an operator"
  (make-instance 'TOKEN :name "id" :value value))

(defun make-operand (value)
  "Make an operand"
  (make-instance 'TOKEN :name "op" :value value))

(defun make-number (value)
  "Make a number"
  (make-instance 'TOKEN :name "number" :value value))

(defun make-whitespace-token ()
  (make-instance 'TOKEN :name "whitespace" :value " "))

(defun interpret-token (token)
  "Return an operator or operand for a token"
  (cond
    ((string= (gethash :start-form *symbols*) token) (make-operator :start-form))
    ((string= (gethash :end-form *symbols*) token) (make-operator :end-form))
    ((string= (gethash :dbl-quote *symbols*) token) (make-operator :dbl-quote))
    ((string= (gethash :single-quote *symbols*) token) (make-operator :sgl-quote))
    ((string= (gethash :+ *symbols*) token) (make-operator :+))
    ((string= (gethash :whitespace *symbols*) token) (make-whitespace-token))
    (t (make-operand token))))

(defun alpha (l)
  (format t "Got to alpha: ~{~A~^, ~}~%" l))

(defun beta (l)
  (format t "Got to beta: ~{~A~^, ~}~%" l))

; find the positions of everything that isn't a symbol and the start ends
(defun second-stage-lexing (lexed)
  "Basically take the first stage lexed and concat all sequential symbols together"
  (let ((data nil) (counter 0))
    (dolist (item lexed)
      (when (string= (name item) "op")
        ; Create a sublist if required
        ; FUTURE NEIL, the problem is in these three lines, dunno why
        ; it just doesn't seem to want to either append to the list, or override it
        (if (nth counter data)
            (alpha (append (nth counter data) `(,item)))
            (beta (append (nth counter data) `(,item)))))

      (unless (string= (name item) "op")
        (setf data (concatenate 'list data `(,item)))
        (incf counter)))

    (format t "~{~A~^, ~}~%" data)
    data))

(defun lexer (line)
  "Turn source code into a token list"
  (let ((tokens (coerce line 'list)))
    (second-stage-lexing (mapcar #'interpret-token tokens))))

(defun ast (lexed-form)
  "Produce an abstract syntax tree from a token list"
  lexed-form)

;; Temp working code
(populate-symbols)
(ast (lexer *example-sum*))
(ast (lexer *example-print*))
