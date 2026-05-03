(course
 (title "Math001")
 (instructors "Joe Smith")
 (semester (ay 2023) (semester Fall))

;; Canvas score items: label, Canvas column heading, max points
 (scores
  (score (name "final exam")   (items "Final exam (111)"))
  (score (name "midterm1")     (items "Midterm 1 (222)"))
  (score (name "midterm2")     (items "Midterm 2 (333)"))
  (score (name "problemsets")
	 (drops 1)
	 (items "ProblemSet 01 (444)"
		"ProblemSet 02 (555)"
		"ProblemSet 03 (666)")))

 ;; Named grading formulas (weighted combinations of score computations)
 (formulas
  (formula (id "S1")
           (component (name "Final")       (Value "final exam")                (weight 0.25))
           (component (name "Max midterm") (Max "midterm1" "midterm2")         (weight 0.25))
           (component (name "Min midterm") (Min "midterm1" "midterm2")         (weight 0.25))
           (component (name "HW")          (Value "problemsets")               (weight 0.25)))
  (formula (id "S2")
           (component (name "Final")       (Value "final exam")                (weight 0.35))
           (component (name "Max midterm") (Max "midterm1" "midterm2")         (weight 0.25))
           (component (name "Min midterm") (Min "midterm1" "midterm2")         (weight 0.15))
           (component (name "HW")          (Value "problemsets")               (weight 0.25)))
  (formula (id "exception")
           (component (name "Final")       (Value "final exam")                (weight 0.50))
           (component (name "Max midterm") (Max "midterm1" "midterm2")         (weight 0.35))
           (component (name "HW")          (Value "problemsets")               (weight 0.15))))

 ;; Grade cutoffs (label, minimum score)
 (grades
  (grade "A+" 98.0)
  (grade "A"  92.5)
  (grade "A-" 89.5)
  (grade "B+" 86.5)
  (grade "B"  82.5)
  (grade "B-" 79.5)
  (grade "C+" 76.5)
  (grade "C"  72.5)
  (grade "C-" 67.5)
  (grade "D+" 65.5)
  (grade "D"  51.5)
  (grade "D-" 50.5))

 (assets-dir "../course-specs/assets/")
 )

