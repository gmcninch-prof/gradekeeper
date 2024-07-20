
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

- a short script for producing a `JSON` description of the students
  and their scores. This is found in the `assets` directory, is
  written in javascript, and depends on the `node` package `csvtojson`.
  
- an `idris2` program `gradekeeper` which takes as input the `JSON`
  course definition and -- using the `JSON` description of students +
  scores -- computes grades and produces a markdown report. 
  
- One can then produce `PDF` and `HTML` versions of the grade report
  using [`pandoc`](http://www.pandoc.org).
  
I'll now give a bit more detailed description of the *example*.

## Assembling the data

`examples/assets` contains a `javascript` script `convert.js`.

In that directory, running

```bash
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

```bash
gradekeeper Mathxyz-definitions.json
```

Using the data in `Mathxyz-grades.json`, it computes scores and grades
for each student, and writes a report in `markdown` format.

## Requirements

The script `convert.js` which *assembles* the data depends on the node
package `csvtojson`.

The main program -- `gradekeeper` -- should be compiled via
[`idris2`](https://www.idris-lang.org/). This repo uses
[`pack`](https://github.com/stefan-hoeck/idris2-pack) for package
management via the `pack.toml` file.

Running `make` should run `pack build gradekeeper.ipkg` which produces
an executable in `build/exec`.

You should be able to test the example via `make examp`.

The `Makefile`  in the example directory runs

```bash
npm install csvtojson
```

That `Makefile` also builds `html` and `pdf` versions of the report
from the `markdown` output, using `pandoc`, which needs to be
available in the `$(PATH)`.


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


