#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#
;;--------------------------------------------------------------------------------
;; Time-stamp: <2024-07-21 Sun 17:19 EDT - george@valhalla>
;;

(use-modules (json)
	     (ice-9 format)
	     (ice-9 getopt-long)
	     (srfi srfi-1))

;; --------------------------------------------------------------------------------

(define (csv-file->scm filename)
  (dsv->scm
   (open-input-file filename)
   #\,
   #:format 'rfc4180
   ))


;; the scm->json procudure wants a *vector* of alist records as its argument
;; 
(define (mk-vect-alist headers contents)
  (list->vector
   (map
    (lambda (c) (map cons headers c))
    contents)))

(define* (extract-headers-mk-vect-alist contents #:key (head 0) (start 1))
  (mk-vect-alist
   (list-ref contents head)
   (drop contents start)))

(define* (csv-file->vect-alist filename #:key (head 0) (start 1))
  (extract-headers-mk-vect-alist
   (csv-file->scm filename)
   #:head head
   #:start start))

(define* (csv-file->json-file csv-filename json-filename #:key (head 0) (start 1))
  (with-output-to-file json-filename
    (lambda ()
      (scm->json (csv-file->vect-alist csv-filename #:head head #:start start)
		 #:pretty #t))))


(define* (mk-map vect-alist field)
  (map
   (lambda (rec) ((cons (assoc-ref rec field) rec)))
   (vector->list vect-alist)
   ))


;; --------------------------------------------------------------------------------

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
      ((pos (if (string? x) (string-contains x " and ") #f)))
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
			     (raw     (string->number (or (assoc-ref crec heading) "0")))
			     (result  (* 100 (/ (or  raw 0) max))))
			  `(("label"   . ,label)
			    ("value"   . ,result))))
		      (vector->list canvas-spec))))
;;    (format (current-error-port) "Name: ~a\n" (assoc-ref rec "Name"))
    `(("name"    . ,(assoc-ref rec "Name"))
      ("id"      . ,(assoc-ref rec "ID"))
      ("email"   . ,(or (assoc-ref rec "Email")""))
      ("level"   . ,(or (assoc-ref rec "Acad Level") (assoc-ref rec "Level") ""))
      ("school"  . ,(or (assoc-ref rec "Program Descr") (assoc-ref rec "Program and Plan") ""))
      ("majors"  . ,majors)      
      ("section" . ,sections)
      ("outcomes". ,(mk-outcomes results)))
    ))

;; --------------------------------------------------------------------------------

(define (main argv)

  (let*
      ((option-spec    '((course     (single-char #\c) (value #t))
			 (canvas     (single-char #\v) (value #t))
			 (enroll     (single-char #\e) (value #t))
			 (output     (single-char #\o) (value #t))))
       (options        (getopt-long argv option-spec))
       (course         (option-ref options 'course  #f))
       (enroll-csv     (option-ref options 'enroll  #f))
       (canvas-csv     (option-ref options 'canvas  #f))
       (course-spec    (if course (get-course-spec course) #f))
       (output         (option-ref options 'output #f)))
    (if course-spec
	(let*
	    ((canvas-spec    (assoc-ref course-spec "CanvasSpec"))
	     (canvas-map     (mk-map
			      (vector->list (csv-file->vect-alist canvas-csv #:start 2))
			      "Integration ID"))
	     (enroll         (csv-file->vect-alist enroll-csv))
	     )
	  ;; (for-each
	  ;;  (lambda (rec)
	  ;;    (format (current-error-port) "~s\n\n" rec))
	  ;;  (vector->list enroll))
	  (if output
	      (with-output-to-file output
		(lambda ()
		  (scm->json
		   (list->vector
		    (map
		     (lambda (rec)
		       (build-student-record canvas-map canvas-spec rec))
		     (vector->list enroll)
		     ))
		   #:pretty #t)))
	      (format #t "No output file specified"))
	  )
	)
    )
  )

;; Local Variables:
;; mode: scheme
;; End:
