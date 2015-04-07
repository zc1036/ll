
(asdf:load-system :cl-pattern)

(defpackage :ll
  (:use :cl))

(in-package :ll)

(defun rulep (x)
    (and (symbolp x) (not (keywordp x))))

(defun parse-try-each-production (alternatives grammar input tree)
    "Attempts to match each alternative in a production rule until one
     succeeds."
    (loop for alt in alternatives
          for success = (parse-impl (append alt grammar) input tree)
          until success
          finally (return success)))

(defun add-nil (list)
    "Adds a NIL to the head of a list, unless there already is one."
    (if (or (null list) (null (car list)))
        list
        (cons nil list)))

(defun push-terminal (term list)
    "Adds a terminal to the parse tree (i.e. pushes the terminal onto
     the CAR list of LIST)."
    (cons (cons term (car list)) (cdr list)))

(defun recursive-reverse (list)
    (let ((newlist (nreverse list)))
        (maplist (lambda (l)
                     (when (and (listp l) (not (null l)) (listp (car l)))
                         (setf (car l) (recursive-reverse (car l)))))
                 newlist)
        newlist))

(defun parse-impl (grammar input tree)
    ;; (format t "matching ~a with ~a (tree = ~a)~%" grammar input tree) ; uncomment for a view of the attempted matches
    (cl-pattern:match (list grammar input)
        ((nil nil) tree)
        (((:split . g-rest) input) (parse-impl g-rest input (add-nil tree)))
        ((? nil) (declare (ignore ?)))
        ((nil ?) (declare (ignore ?)))
        (((g-head . g-rest) (i-head . i-rest))
             (cond
               ((rulep g-head) (parse-try-each-production (funcall g-head)
                                                          (cons :split g-rest)
                                                          input
                                                          (add-nil tree)))
              ((equal g-head i-head) (parse-impl g-rest i-rest (push-terminal i-head tree)))))))

(defun parse (grammar input)
    "Parses a list of tokens according to the given grammar."
    (cl-pattern:match (parse-impl grammar input nil)
        ((nil . rest) (recursive-reverse rest))
        (clean (recursive-reverse clean))))

(defun match-try-each-production (alternatives grammar input)
    (loop for alt in alternatives
          for success = (match (append alt grammar) input)
          until success
          finally (return success)))

(defun match (grammar input)
    "If the given input matches the given grammar rules, return T;
     otherwise, return NIL."
    (cl-pattern:match (list grammar input)
        ((nil nil) t)
        ((? nil) (declare (ignore ?)))
        ((nil ?) (declare (ignore ?)))
        (((g-head . g-rest) (i-head . i-rest))
             (cond
              ((rulep g-head) (match-try-each-production (funcall g-head) g-rest input))
              ((equal g-head i-head) (match g-rest i-rest))))))

;;; It's actually not too bad without cl-pattern but I prefer pattern matching
;;; because it's more understandable without so many CARs and CDRs.
;; (defun match (grammar input)
;;     (cond
;;      ((and (null grammar) (null input)) t)
;;      ((or (null grammar) (null input)) nil)
;;      ((equal (car grammar) (car input)) (match (cdr grammar) (cdr input)))
;;      ((rulep (car grammar)) (try-match (funcall (car grammar)) (cdr grammar) input))
;;      (t nil)))
