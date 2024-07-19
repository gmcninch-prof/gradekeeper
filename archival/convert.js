#!/usr/bin/env node

const fs = require('fs');

const process = require('process');
const path = require('path');

const myArgs = process.argv.slice(2);
const courseFile = myArgs[0]

const course = JSON.parse(fs.readFileSync(courseFile, 'utf8'));

// extension should include the dot, for example '.html'
function changeExtension(file, extension) {
  const basename = path.basename(file, path.extname(file))
  return path.join(path.dirname(file), basename + extension)
}

const canvasCSVFile = path.join(course.dataDir,course.canvasCSVFile)
const courseJSONFile = path.join(course.dataDir,course.courseJSONFile)
const enrollmentCSV = path.join(course.dataDir,course.enrollmentFile)


function avg(score,max) {
  return 100.0*score/max || 0
}


function studentData(canvasmap) {
  return (rec)=> {
    const main =  { "name": rec.Name,
		    "id": rec.ID,
		    "email": rec["Email"] || "",
		    "level": rec["Acad Level"] || rec["Level"],
		    "school": rec["Program Descr"] || rec["Program and Plan"],
		    "majors": (rec["Plan(s)"] || rec["Program and Plan"]).split(", ")
		    
		  }
    const canvas = canvasmap.get(main.id);
    if (canvas) {
      return { ...main,
	       ...canvas
	     }
    }
    else {
      console.error("failed for" + main.name);
    }
  }
}

function canvasData(rec) {
  return [ rec["Integration ID"],
	   { "section": rec.Section.split(" and "),
	     "outcomes": course.CanvasSpec.map((out)=>({ "label": out.label,
							 "value": val(rec,out)
						       }
						      )
					      )
	   }
	 ]
}


function val(rec,out) {
  if (typeof out.value === 'object')
    return {"scores": out.value.scores.map((x)=>avg(rec[x.value],x.max))}
  else
    return {"score": avg(rec[out.value],out.max)}
}


function keep(rec) {
  return rec != null 
}


const csv=require('csvtojson')

function mkRecords(canvasmap) {
  console.log("Creating " +  courseJSONFile)
  
  csv({})
    .fromFile(enrollmentCSV)
    .then((jsonObj)=>{
      results=jsonObj.map(studentData(canvasmap)).filter(keep);
      fs.writeFile(courseJSONFile, JSON.stringify(results,null,2), err => {
	if (err) {
	  console.error(err);
	}
	// file written successfully
      });
    })
}



let create = csv().fromFile(canvasCSVFile)
  .then((jsonObj)=>{
    canvasrec = jsonObj.map(canvasData);
    canvasMap = new Map (canvasrec);
    mkRecords(canvasMap)
  })





		
  
    
