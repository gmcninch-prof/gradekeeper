#!/usr/bin/env -S guile --no-auto-compile
!#

;;; course-spec-to-json.scm
;;; Convert s-expression course spec to JSON for gradekeeper/idris consumption.
;;;
;;; Usage: course-spec-to-json.scm <spec.scm> [--output <out.json>]

(use-modules (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-11))

;;; ---------------------------------------------------------------------------
;;; Minimal JSON emitter
;;; ---------------------------------------------------------------------------

(define (json-string s)
  ;; Emit a JSON string, escaping as needed.
  (string-append
   "\""
   (list->string
    (append-map (lambda (c)
                  (cond ((char=? c #\") '(#\\ #\"))
                        ((char=? c #\\) '(#\\ #\\))
                        ((char=? c #\newline) '(#\\ #\n))
                        ((char=? c #\tab) '(#\\ #\t))
                        (else (list c))))
                (string->list s)))
   "\""))

(define (json-number n)
  ;; Idris JSON expects doubles; ensure we always emit a decimal point.
  (let ((s (number->string (exact->inexact n))))
    ;; Guile may emit "98." — normalize to "98.0"
    (if (string-suffix? "." s)
        (string-append s "0")
        s)))

(define (json-bool b)
  (if b "true" "false"))

(define* (json-object pairs #:optional (indent 0))
  ;; pairs : list of (key . json-value-string)
  (if (null? pairs)
      "{}"
      (let ((inner-indent (make-string (+ indent 2) #\space))
            (outer-indent (make-string indent #\space)))
        (string-append
         "{\n"
         (string-join
          (map (lambda (p)
                 (string-append inner-indent
                                (json-string (car p))
                                ": "
                                (cdr p)))
               pairs)
          ",\n")
         "\n"
         outer-indent
         "}"))))

(define* (json-array items #:optional (indent 0))
  (if (null? items)
      "[]"
      (let ((inner-indent (make-string (+ indent 2) #\space))
            (outer-indent (make-string indent #\space)))
        (string-append
         "[\n"
         (string-join
          (map (lambda (i) (string-append inner-indent i)) items)
          ",\n")
         "\n"
         outer-indent
         "]"))))

;;; ---------------------------------------------------------------------------
;;; Helpers for extracting fields from a sexp record
;;;
;;; A "record" here is a list like:
;;;   (score (name "homework") (drops 1) (items "ps1" "ps2"))
;;; We use assoc on the cdr.
;;; ---------------------------------------------------------------------------

(define (field-ref rec key)
  ;; Return the cdr of the first sublist whose car is key, or #f.
  (let ((pair (assq key (cdr rec))))
    (if pair (cdr pair) #f)))

(define (field-ref1 rec key)
  ;; Like field-ref but returns the single value (cadr of the sublist).
  (let ((pair (assq key (cdr rec))))
    (if pair (cadr pair) #f)))

;;; ---------------------------------------------------------------------------
;;; Computation (FormulaComponent.component) -> JSON
;;;
;;; Sexp forms:
;;;   (Value "scoreName")
;;;   (Max "s1" "s2")
;;;   (Min "s1" "s2")
;;;   (Maxl "s1" "s2" ...)
;;;   (Minl "s1" "s2" ...)
;;;
;;; ObjectWithSingleField JSON encoding:
;;;   {"Value": ["scoreName"]}
;;;   {"Max": ["s1", "s2"]}
;;;   etc.
;;; ---------------------------------------------------------------------------


(define (computation->json comp)
  (match comp
    (('Value arg)
     (json-object `(("Value" . ,(json-string arg)))))
    (('Max s1 s2)
     (json-object `(("Max" . ,(json-array (list (json-string s1) (json-string s2)))))))
    (('Min s1 s2)
     (json-object `(("Min" . ,(json-array (list (json-string s1) (json-string s2)))))))
    (('Maxl args ...)
     (json-object `(("Maxl" . ,(json-array (map json-string args))))))
    (('Minl args ...)
     (json-object `(("Minl" . ,(json-array (map json-string args))))))
    (_ (error "Unknown computation form" comp))))

;;; ---------------------------------------------------------------------------
;;; FormulaComponent -> JSON
;;;
;;; Sexp: (component (name "Final") (Value "finalexam") (weight 0.25))
;;; The computation is whichever sublist has car in
;;; {Value, Max, Min, Maxl, Minl}.
;;; ---------------------------------------------------------------------------

(define computation-tags '(Value Max Min Maxl Minl))

(define (find-computation component-sexp)
  ;; Find the computation subform within a component sexp.
  (let loop ((rest (cdr component-sexp)))
    (cond
     ((null? rest) (error "No computation found in component" component-sexp))
     ((memq (caar rest) computation-tags) (car rest))
     (else (loop (cdr rest))))))

(define (formula-component->json fc)
  (let* ((comp-name (field-ref1 fc 'name))
         (weight    (field-ref1 fc 'weight))
         (comp-form (find-computation fc)))
    (json-object
     `(("compName"  . ,(json-string comp-name))
       ("component" . ,(computation->json comp-form))
       ("weight"    . ,(json-number weight))))))

;;; ---------------------------------------------------------------------------
;;; Formula -> JSON
;;;
;;; Sexp: (formula (id "S1") (component ...) (component ...) ...)
;;; ---------------------------------------------------------------------------

(define (formula->json f)
  (let* ((id         (field-ref1 f 'id))
         (components (filter (lambda (x) (and (pair? x) (eq? (car x) 'component)))
                             (cdr f))))
    (json-object
     `(("id"          . ,(json-string id))
       ("formulaComps" . ,(json-array (map formula-component->json components)))))))

;;; ---------------------------------------------------------------------------
;;; Score -> JSON
;;;
;;; Sexp: (score (name "homework") (drops 1) (items "ps1" "ps2" ...))
;;; drops is optional.
;;; ---------------------------------------------------------------------------

(define (score->json s)
  (let* ((name  (field-ref1 s 'name))
         (drops (field-ref1 s 'drops))
         (items (field-ref  s 'items))
         (base  `(("scoreName" . ,(json-string name))
                  ("items"     . ,(json-array (map json-string items))))))
    (json-object
     (if drops
         (append base `(("drops" . ,(number->string drops))))
         base))))

;;; ---------------------------------------------------------------------------
;;; Grade -> JSON
;;;
;;; Sexp:
;;;   (grade "A+" 98.0)  -> {"MkGrade": {"label": "A+", "min": 98.0}}
;;;   Failing            -> "Failing"
;;;   Incomplete         -> "Incomplete"
;;; ---------------------------------------------------------------------------

(define (grade->json g)
  (cond
   ((eq? g 'Failing)    (json-string "Failing"))
   ((eq? g 'Incomplete) (json-string "Incomplete"))
   ((and (pair? g) (eq? (car g) 'grade))
    (let ((label (cadr g))
          (min   (caddr g)))
      (json-object
       `(("MkGrade" . ,(json-object `(("label" . ,(json-string label))
                                      ("min"   . ,(json-number min)))))))))
   (else (error "Unknown grade form" g))))

;;; ---------------------------------------------------------------------------
;;; StudentException -> JSON
;;;
;;; Sexp: (exception (name "Maggie Olson") (id "1422133")
;;;                  (formulas "Ex1" "Ex2") (incomplete #f)
;;;                  (comment "..."))         ; comment optional
;;; ---------------------------------------------------------------------------

(define (exception->json e)
  (let* ((name      (field-ref1 e 'name))
         (id        (field-ref1 e 'id))
         (formulas  (field-ref  e 'formulas))
         (incomplete (field-ref1 e 'incomplete))
         (comment   (field-ref1 e 'comment))
         (base      `(("name"       . ,(json-string name))
                      ("id"         . ,(json-string id))
                      ("formulas"   . ,(json-array (map json-string formulas)))
                      ("incomplete" . ,(json-bool incomplete))
                      ("comment"    . ,(if comment (json-string comment) "null")))))
    (json-object base)))

;;; ---------------------------------------------------------------------------
;;; Semester -> JSON
;;;
;;; Sexp: (semester (ay 2023) (semester Fall))
;;; ---------------------------------------------------------------------------

(define (semester->json s)
  (let ((ay  (field-ref1 s 'ay))
        (sem (field-ref1 s 'semester)))
    (json-object
     `(("ay"       . ,(number->string ay))
       ("semester" . ,(json-string (symbol->string sem)))))))

;;; ---------------------------------------------------------------------------
;;; Top-level course -> JSON
;;; ---------------------------------------------------------------------------

(define (course->json spec)
  ;; spec is the full (course ...) sexp
  (let* ((title         (field-ref1 spec 'title))
         (instructors   (field-ref  spec 'instructors))
         (semester-sexp (assq 'semester (cdr spec)))
         (scores-sexp   (cdr (assq 'scores (cdr spec))))
         (formulas-sexp (let ((f (assq 'formulas (cdr spec))))
                          (if f (filter (lambda (x) (and (pair? x) (eq? (car x) 'formula)))
                                        (cdr f))
                              '())))
         (except-formulas-sexp (let ((f (assq 'except-formulas (cdr spec))))
                                 (if f (filter (lambda (x) (and (pair? x) (eq? (car x) 'formula)))
                                               (cdr f))
                                     '())))
         (grades-sexp   (let ((g (assq 'grades (cdr spec))))
                          (if g (cdr g) #f)))
         (exceptions-sexp (let ((e (assq 'exceptions (cdr spec))))
                            (if e (filter (lambda (x) (and (pair? x) (eq? (car x) 'exception)))
                                          (cdr e))
                                '())))

         (fields
          `(("title"          . ,(json-string title))
            ("instructors"    . ,(json-array (map json-string instructors)))
            ("semester"       . ,(semester->json semester-sexp))
            ("scores"         . ,(json-array (map score->json scores-sexp)))
            ("formulas"       . ,(json-array (map formula->json formulas-sexp)))
            ("exceptFormulas" . ,(json-array (map formula->json except-formulas-sexp)))
            ("grades"         . ,(if grades-sexp
                                     (json-array (map grade->json grades-sexp))
                                     "null"))
            ("exceptions"     . ,(json-array (map exception->json exceptions-sexp))))))

    (json-object fields)))

;;;--------------------------------------------------------------------------------
;;; emit course.mk

(define (course-mk spec)
  (let*
      ((semester-sexp (assq 'semester (cdr spec)))
       (ay (field-ref1 semester-sexp 'ay))
       (sem (field-ref1 semester-sexp 'semester))
       (title (field-ref1 spec 'title))
       (instructors (field-ref spec 'instructors))
       (assets-dir (field-ref1 spec 'assets-dir)))
(with-output-to-file "course.mk"
  (lambda ()
    (display
     (string-append
      (string-join
       `(,(string-append "COURSE      := " title)
	 ;; AY formatted as "YYYY-YYYY+1" to match gradekeeper markdown output naming
	 ,(format #f "AY          := ~a-~a" ay (+ 1 ay))	 
	 ,(string-append "SEMESTER    := " (symbol->string sem))
	 ,(string-append "INSTRUCTORS := " (string-join instructors ", ")))
       "\n")
      "\n"))
    (when assets-dir
      (display (string-append "ASSETS_DIR   := " assets-dir "\n")))))))

;;;--------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Main
;;; ---------------------------------------------------------------------------

(define (main args)
  (let loop ((rest (cdr args))
             (input-file #f)
             (output-file #f))
    (match rest
      (()                                        ; ← base case: no more args
       (unless input-file
         (error "Usage: course-spec-to-json.scm <spec.scm> [--output <out.json>]"))
       (let* ((spec (call-with-input-file input-file read))
              (json (course->json spec)))
         (course-mk spec)
         (if output-file
             (call-with-output-file output-file
               (lambda (port) (display json port) (newline port)))
             (begin (display json) (newline)))))
      (("--output" out . rest*)
       (loop rest* input-file out))
      ((f . rest*)
       (loop rest* f output-file)))))

(main (command-line))
