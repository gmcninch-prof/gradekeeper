
# gradekeeper

Compute grades for a class.


There is a (hopefully) working example in the `examples` directory.


## Assembling the data

`examples/assets` contains a `javascript` script `convert.js`.

In that directory, running

```
convert.js Mathxyz-definitions.json
```

produces `Mathxyz-grades.json` using data from `Mathxyz-enrollment.csv` and
`Mathxyz-canvas-grades.csv`.

At Tufts, `Mathxyz-enrollment.csv` is the `csv` representation of the
class that one gets from `SIS`, and `Mathxyz-canvas-grades.csv` is
what one gets upon `export`ing course grades from `Canvas`.

The `definitions` file `Mathxyz-definitions.json` includes
specifications for the course:

  - In this case, it tells us that the `strategy` for computing grade
    for the `problemsets` is "`dropandaverage`" (i.e. drop the lowest
    grade and return the average).
	
  - It gives us two grading formulas `S1` and `S2`. 
  
    
	The field 
	```
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
	
	```
	"grades": null
	```
	
	and the `standard` grading scheme - which appears as a `term` in the idris code
	(see `src/LetterGrades.idr`)
	
  - Finally, the definitions file indicates data about the grades specified in the Canvas data 
    (`CanvasSpec`)
	
	For example,
	
	```
	...
	{ "label": "midterm2",
      "value": "Midterm 2 (333)",
      "max": 100
    },
    { "label": "problemsets",
      "value": { "scores":
		 [ {"value": "ProblemSet 01 (444)",
		    "max": 40.0
		   },
		   {"value": "ProblemSet 02 (555)",
		    "max": 35.0
		   },
		   {"value": "ProblemSet 03 (666)",
		    "max": 35.0
		   }
		 ]
	       }
    }
	...
	```

    Determines labels `midterm2` and `problemsets` (which are used in
    the `grading formulas` mentioned earlier). The `value` parameter
	indicates the field name in the data from canvas (in this case, 
	the field names found in `Mathxyz-canvas-grades.csv`).
    


## Producing the grade report

Now,  the `idris2` program `gradekeeper` is run 

```
gradekeeper Mathxyz-definitions.json
```

Using the data in `Mathxyz-grades.json`, it computes scores and grades
for each student, and writes a report in `markdown` format.

## Requirements

The script `convert.js` which *assembles* the data depends on the node
package `csvtojson`.

The `Makefile`  in the example directory runs

```
npm install csvtojson
```

That `Makefile` also builds `html` and `pdf` versions of the report
from the `markdown` output, using `pandoc`, which needs to be
available in the `$(PATH)`.

