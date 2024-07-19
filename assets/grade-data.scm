#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#
;;--------------------------------------------------------------------------------
;; Time-stamp: <2024-07-18 Thu 14:49 EDT - george@valhalla>
;;

(use-modules (csv-to-json)
	     (json)
	     (ice-9 format)
	     (ice-9 getopt-long)
	     (srfi srfi-1))


;; --------------------------------------------------------------------------------


(define (last sexp)
  (car (reverse sexp)))

(define (initial sexp)
  (reverse (cdr (reverse sexp))))

(define (snoc xs x)
  (reverse (cons x (reverse xs))))

(define (normalize path)
  "Remove trailing `file-name-seperator` from a file-path, if it is present"
  (let
      ((cs (string->list path))
       (sep (car (string->list file-name-separator-string)))) ;; file-name-seperator-string as a `char`
    (if (eq? (last cs) sep)
	(list->string (initial cs))
	path))) 

(define (mk-path ps)
  (string-join
   (map normalize ps)
   file-name-separator-string))

;; --------------------------------------------------------------------------------

(define (get-course-spec ps)
  (with-input-from-file ps
    (lambda () (json->scm))))

(define (split-and x)
  (let*
      ((pos (string-contains x " and ")))
    (if pos
	(let
	    ((start (substring x 0 pos))
	     (rest (split-and (substring x (+ 5 pos)))))
	  (cons start rest))
	(list x)
	)
    ))

(define (canvas-lookup rec canvas-map)
  (let
      ((id (assoc-ref rec "ID")))
    (assoc-ref canvas-map id)))


(define (mk-outcomes results)
  (let
      ((labels (delete-duplicates (map (lambda (r) (assoc-ref r "label")) results))))
    (list->vector
     (map
      (lambda (lab)
	(let
	    ((lresults (filter (lambda (r) (equal? lab (assoc-ref r "label"))) results)))
	  `(("label" . ,lab)
	    ("value" . 
	     ,(if (= 1 (length lresults))
		  `(("score"  . ,(assoc-ref (car lresults) "value")))
		  `(("scores" . ,(list->vector (map (lambda (r) (assoc-ref r "value")) lresults))))
		  ))))
	)
      labels))
    ))


(define (build-student-record canvas-map canvas-spec rec)
  (let*
      ((crec (canvas-lookup rec canvas-map))
       (sections (list->vector (split-and (assoc-ref crec "Section"))))
       (majors   (list->vector (map string-trim-both
				    (string-split
				     (or (assoc-ref rec "Plan(s)") (assoc-ref rec "Program and Plan"))
				     #\,))))
       (results  (map (lambda (c)
			(let*
			    ((label (assoc-ref c "label"))
			     (heading (assoc-ref c "heading"))
			     (max     (assoc-ref c "max"))
			     (raw     (or (string->number (assoc-ref crec heading)) 0))
			     (result (* 100 (/ raw max))))
			  `(("label"   . ,label)
			    ("value"   . ,result))))
		      (vector->list canvas-spec))))
    `(("name"    . ,(assoc-ref rec "Name"))
      ("id"      . ,(assoc-ref rec "ID"))
      ("email"   . ,(assoc-ref rec "Email"))
      ("level"   . ,(assoc-ref rec "Acad Level"))
      ("school"  . ,(assoc-ref rec "Program Descr"))
      ("majors"  . ,majors)      
      ("section" . ,sections)
      ("outcomes". ,(mk-outcomes results)))
    ))

;; --------------------------------------------------------------------------------

(define (main argv)

  (let*
      ((option-spec    '((course     (single-char #\c) (value #t))
			 (canvas     (single-char #\v) (value #t))
			 (enroll     (single-char #\e) (value #t))))
       (options        (getopt-long argv option-spec))
       (course         (option-ref options 'course  #f))
       (enroll-csv     (option-ref options 'enroll  #f))
       (canvas-csv     (option-ref options 'canvas  #f))
       (course-spec    (if course (get-course-spec course) #f)))
    (if course-spec
	(let*
	    ((canvas-spec    (assoc-ref course-spec "CanvasSpec"))
	     (canvas-map     (map
			      (lambda (rec) (cons (assoc-ref rec "Integration ID") rec))
			      (vector->list (csv-file->sxml canvas-csv #:start 2))))
	     (enroll         (csv-file->sxml enroll-csv))
	     (rec            (build-student-record canvas-map canvas-spec (vector-ref enroll 3)))
	     )
	  (scm->json
	   (list->vector
	    (map
	     (lambda (rec)
	       (build-student-record canvas-map canvas-spec rec))
	     (vector->list enroll)
	     ))
	   #:pretty #t)
	  )
	)
    )
  )
