(use-modules (dsv)
	     (json)
	     (srfi srfi-1)
	     (srfi srfi-43))

(define (csv-file->scm filename)
  (dsv->scm
   (open-input-file filename)
   #\,
   #:format 'rfc4180
   ))


;; the scm->json procudure wants a *vector* of sxml records as its argument
;; 
(define (mk-sxml headers contents)
  (list->vector
   (map
    (lambda (c) (map cons headers c))
    contents)))

(define* (mk-sxml-by-num contents #:key (head 0) (start 1))
  (mk-sxml
   (list-ref contents head)
   (drop contents start)))

(define* (csv-file->sxml filename #:key (head 0) (start 1))
  (mk-sxml-by-num
   (csv-file->scm filename)
   #:head head
   #:start start))

(define* (csv-file->json-file csv-filename json-filename #:key (head 0) (start 1))
  (with-output-to-file json-filename
    (lambda ()
      (scm->json (csv-file->sxml csv-filename #:head head #:start start) #:pretty #t)
      )
    )
  )



;; (define teaching-dir "/home/george/Prof-Teach/scores/")


;; (define canvas-csv-file
;;   (mk-path
;;    `(,teaching-dir
;;      "AY2023-2024--2024-sp--Math190-data"
;;      "2024-05-11T2025_Grades-Sp24-MATH-0190-01-Advanced_Special_Topics.csv")))

;; (define enrollment-csv-file
;;   (mk-path
;;    `(,teaching-dir
;;      "AY2023-2024--2024-sp--Math190-data"			       
;;      "Spring2024-Math190-enrollment-2024-05-10.csv")))


;; (define canvas-csv-file
;;   (mk-path
;;    `(,teaching-dir
;;      "AY2023-2024--2023-fa--Math051-data"
;;      "2024-02-05T1600_Grades-Fa23-MATH-0051-01-Differential_Equations.csv")))

;; (define enrollment-csv-file
;;   (mk-path
;;    `(,teaching-dir
;;      "AY2023-2024--2023-fa--Math051-data"			       
;;      "AY2023-2024--2023-fa--Math051-enrollment.csv")))

;; (csv-file->json-file canvas-csv-file
;; 		     "canvas.json"
;; 		     #:start 2)

;; (csv-file->json-file enrollment-csv-file
;; 		     "enrollment.json")


