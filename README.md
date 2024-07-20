
# gradekeeper

Compute grades for a class.


There is a (hopefully) working example in the `examples` directory.


## "What does this thing do, and what do I need to do to get it working?"

In the unlikely event that anyone tries to use this code, here is a
quick overview for how I use it for a given course.

- get the *enrollment data* from `Tufts SIS`, as a csv file.

- get the *grading info* from `Canvas`, as a csv file.

Students appear in each. Names don't necessarily match (sometimes
Canvas seems to use nicknames). Each record in *enrollment data* has
an `id`, and each record in *canvas data* has several `id`s, and one
of them -- `Integration ID' -- matches the *enrollment data*.

With these files in hand, one must now write the `JSON` course definitions
file. There is an example (discussed below) in the `example` directory.

There are two different functions provided by the code in this repo.

- a `guile-scheme` script `grade-data.scm` for producing a `JSON`
  description of the students and their scores. This is found in the
  `assets` directory. It depends on a few `guile` libraries:
  `guile-dsv` and `guile-json`
  
- an `idris2` program `gradekeeper` which takes as input the `JSON`
  course definition and -- using the `JSON` description of students +
  scores -- computes grades and produces a markdown report. 
  
- One can then produce `PDF` and `HTML` versions of the grade report
  using [`pandoc`](http://www.pandoc.org).
  
I'll now give a bit more detailed description of the *example*


## Assembling the data and producing the report

The recommendation is to copy/adapt the `Makefile` in the `examples`
directory.  It invokes `grade-data.scm` to produce the "data `json`"
output.

In that directory, `Mathxyz-enrollment.csv` is the `csv`
representation of the class that one gets (at Tufts) from `SIS`, and
`Mathxyz-canvas-grades.csv` is what one gets upon `export`ing course
grades (at Tufts) from `Canvas`.

The `definitions` file `Mathxyz-definitions.json` includes
specifications for the course:

  - In this case, it tells us that the `strategy` for computing grade
    for the `problemsets` is "`dropandaverage`" (i.e. drop the lowest
    grade and return the average).
	
  - It gives us two grading formulas `S1` and `S2`. 
  
    
	The field 
	```JSON
	"gradingFormulas": ["S1","S2"]
	```
	indicates that by default, a students course
    score will then be the *maximum* of the scores determined by `S1`
    and by `S2`.
	
    It also defines an `exceptional` grading formula `except`. To
	apply an exceptional formula, a student must appear in the
	`exceptionsFile`, in this case `grade-exceptions.json`.  It then
	*over-rides* the `gradingFormulas` field for that indicated
	student.
	
  - It includes a specification (`grades`) for turning numerical
    scores into letter grades (you can instead set
	
	```JSON
	"grades": null
	```
	
	and the `standard` grading scheme - which appears as a `term` in the idris code
	(see `src/LetterGrades.idr`)
	
  - Finally, the definitions file indicates data about the grades specified in the Canvas data 
    (`CanvasSpec`)
	
	For example,
	
	```JSON
	...
	[{ "label": "midterm2",
       "heading": "Midterm 2 (333)",
       "max": 100
     },
     { "label": "problemsets",
	   "heading": "ProblemSet 01 (444)",
	   "max": 40.0
     },
	 { "label": "problemsets",
	   "heading": "ProblemSet 02 (555)",
	   "max": 35.0
     },
	 { "label": "problemsets",
       "heading": "ProblemSet 03 (666)",
	   "max": 35.0
     }
    ]
	...
	```

    determines labels `midterm2` and `problemsets` (which are used in
    the `grading formulas` mentioned earlier). The `heading` parameter
	indicates the field name in the data from canvas (in this case, 
	the field names found in `Mathxyz-canvas-grades.csv`).
    

Now, the `Makefile` invokes `grade-data.scm` via the following stanza:

```
CONVERT=/home/george/.local/bin/grade-data.scm

$(COURSE_DATA): $(COURSE_SPEC) $(ENROLL_CSV) $(CANVAS_CSV)
	@unix2dos -q $(ENROLL_CSV)
	@unix2dos -q $(CANVAS_CSV)
	$(CONVERT) --enroll $(ENROLL_CSV) --course $(COURSE_SPEC) --canvas $(CANVAS_CSV) --output $(COURSE_DATA)
```

For the correct operation of the `(guile dsv)` module, the `csv` files
need to be in `DOS` format -- i.e. they need to have `CRLF`
end-of-lines (`\r\n` instead of just `\n`). So the code above makes
sure this is the case using the script `unix2dos`.

Now we invoke the `grade-data.scm` on the two `csv` files together with the json `spec` file.

## Producing the grade report

Now,  the `Makefile` also runs the `idris2` program `gradekeeper` 

```bash
gradekeeper Mathxyz-definitions.json
```

Using the data in `Mathxyz-grades.json`, it computes scores and grades
for each student, and writes a report in `markdown` format.

In the `Makefile` this is invoked by the stanza:

```
GK=gradekeeper
targets_md=Mathxyz-2023-2024-Fall.md

$(targets_md): $(COURSE_SPEC) $(COURSE_DATA)
	$(GK) --spec $(COURSE_SPEC) --data $(COURSE_DATA)
```

Now the `html` and `pdf` are produced from the `md` via `pandoc`.

## Requirements

The code depends on `idris2` and various libraries (installed via
`pack`), `guile`, the `guile` libraries `(guile dsv)` and `(guile json)`, and the script
`unix2dos` (which I installed via `guix`: `guix install unix2dos`).

## Some discussion of `Formula` specification in the course definition `JSON`

Here is an example `formula` in the `JSON` course definition:

```JSON
    { "id": "S1",
      "formula": [ {"compName": "Final",
		    "computation":{"copy": ["final exam"]},
		    "weight": 0.25},
		   {"compName": "Max midterm",
		    "computation":{"max": [["midterm1", "midterm2"]]},
		    "weight": 0.25},
		   {"compName": "Min midterm",
		    "computation":{"min": [["midterm1", "midterm2"]]},
		    "weight": 0.25},
		   {"compName": "HW",
		    "computation":{"copy": ["problemsets"]},
		    "weight": 0.25}
		 ]
    },
```

Some of the structure perhaps feels a bit odd. The point is that this is parsed to a
`Formula` data structure in `idris`, as follows:

```idris
data Computation = Copy String
                 | Max (List String) 
                 | Min (List String) 

record ScoreComponent where
  constructor MkScoreComponent
  compName : String
  computation: Computation
  weight : Double
  
record Formula where
  constructor MkFormula
  id : String
  formula :  List ScoreComponent
```

So the above `JSON` defines a `Formula` whose
`id` is `"S1"`, and whose `formula` component is a list of `ScoreComponent`s.

Each `ScoreComponent` has a `compName` (a `string`), a `computation`
(a `Computation`) and a `weight` (a `double`)

The `Computation` type is a sum type, and this is the origin of some
of the slightly mysterious looking `JSON` constructions.

The `JSON` specification `"computation":{"copy": ["final exam"]}`
becomes the term `Copy "final exam"` of type `Computation`.

Here `final exam` is a `label` for an `Outcome` recorded in the course
scores, and `Copy` just means to use the numeric value associated to
that `Outcome` label.

In the `JSON` description, the string `"final exam"` needed to be
wrapped as a list because the `Copy` constructor could have taken more
than one argument.

On the other hand, the `JSON` specification 

```JSON
{ "compName": "Max midterm",
  "computation":{"max": [["midterm1", "midterm2"]]},
  "weight": 0.25}
```

becomes the term `Max ["midterm1", "midterm2"]` of type `Computation`.

Again, in the `JSON` description, the list `["midterm1", "midterm2"]`
needed to be wrapped as a list (thus producing the "double list")
because the `Max` constructor could have taken more than one argument.


