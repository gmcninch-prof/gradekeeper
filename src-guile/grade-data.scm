#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#
;;--------------------------------------------------------------------------------
;; Time-stamp: <2024-08-24 Sat 11:44 EDT - george@valhalla>
;;

(use-modules (json)
	     (dsv)
	     (ice-9 format)
	     (ice-9 getopt-long)
	     (ice-9 pretty-print)
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
(define (mk-alist headers contents)
  (map
   (lambda (c) (map cons headers c))
   contents))

(define* (extract-headers-mk-vect-alist contents #:key (head 0) (start 1))
  (mk-alist
   (list-ref contents head)
   (drop contents start)))

(define* (csv-file->alist filename #:key (head 0) (start 1))
  (extract-headers-mk-vect-alist
   (csv-file->scm filename)
   #:head head
   #:start start))

(define* (csv-file->json-file csv-filename json-filename #:key (head 0) (start 1))
  (with-output-to-file json-filename
    (lambda ()
      (scm->json (list->vector (csv-file->alist csv-filename #:head head #:start start))
		 #:pretty #t))))


(define* (mk-map alist field)
  (map
   (lambda (rec) (cons (assoc-ref rec field) rec))
   alist))


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

(define (get-score maxes record heading)
  (let
      ((max (or (string->number (assoc-ref maxes heading))  100))
       (raw (or (string->number (assoc-ref record heading)) 0)))
;;    (format #t "raw ~a max ~a" raw max)
    (* 100 (/ raw max))
    ))

(define (build-student-record maxes canvas-map score-specs rec)
  (let*
      ((crec (canvas-lookup rec canvas-map))
       (sections (list->vector (split-and (assoc-ref crec "Section"))))
       (majors   (list->vector (map string-trim-both
				    (string-split
				     (or (assoc-ref rec "Plan(s)") (assoc-ref rec "Program and Plan"))
				     #\,))))
       (outcomes  (list->vector
		   (map (lambda (score-spec)
			  (let*
			      ((score-name (assoc-ref score-spec "scoreName"))
			       (items (assoc-ref score-spec "items"))
			       (marks (map
				       (lambda (heading) (get-score maxes crec heading))
				       (vector->list (assoc-ref score-spec "items")))))
			    `(("scoreName" . ,score-name)
			      ("marks"    . ,(list->vector marks)))))
			(vector->list score-specs)))))
    ;;    (format (current-error-port) "Name: ~a\n" (assoc-ref rec "Name"))
    `(("name"     . ,(assoc-ref rec "Name"))
      ("id"       . ,(assoc-ref rec "ID"))
      ("email"    . ,(or (assoc-ref rec "Email")""))
      ("level"    . ,(or (assoc-ref rec "Acad Level") (assoc-ref rec "Level") ""))
      ("school"   . ,(or (assoc-ref rec "Program Descr") (assoc-ref rec "Program and Plan") ""))
      ("majors"   . ,majors)      
      ("section"  . ,sections)
      ("outcomes" . ,outcomes))
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
	    ((scores-specs   (assoc-ref course-spec "scores"))
	     (canvas-alist   (csv-file->alist canvas-csv #:start 2))
	     (maxes          (car canvas-alist))
	     (canvas-map     (mk-map
			      (cdr canvas-alist)
			      "Integration ID"))
	     (enroll-data    (csv-file->alist enroll-csv))
	     )
	  ;; (for-each
	  ;;  (lambda (rec)
	  ;;    (format (current-error-port) "~s\n\n" rec))
	  ;;  (vector->list enroll-data))
	  (if output
	      ;; (for-each
	      ;;  (lambda (rec)
	      ;; 	 (format #t "result for rec ~s\n" rec)
	      ;; 	 (pretty-print(build-student-record maxes canvas-map scores-specs rec)))
	         
	      ;;  enroll-data)
		  
	      (with-output-to-file output
		(lambda ()
		  (scm->json
		   (list->vector
		    (map
		     (lambda (rec)
		       (build-student-record maxes canvas-map scores-specs rec))
		     enroll-data))
		   #:pretty #t)))
	      (format #t "No output file specified")
	      
	  )))))

;; Local Variables:
;; mode: scheme
;; End:
