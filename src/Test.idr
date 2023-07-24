module Test

import Course
import Student
import LetterGrades 

c : Course
c = MkCourse { title = "Example Course"
             , semester = MkAcademicSemester { ay = MkAY 2023, semester= Spring }
             , strategies = [ MkComputeStrategy "problemsets" (DropAndAverage 2) ]
             , formulas = [ MkFormula { id = "S1" 
                                      , formula = [ Copy "Final"  "final exam" 0.3
                                                  , Copy "Midterm1" "midterm1" 0.3
                                                  , Copy "Midterm2" "midterm2"  0.3
                                                  , Copy "problemsets" "problemsets" 0.1
                                                  ]
                                      }
                          ]
             , sections = []
             , grades = Nothing
             }


s : StudentSISData
s = MkStudentSISData { name = "Acharya, Aditya"
                     , section = "Sp23-MATH-0135-01-Real Analysis I"
                     , outcomes = [ MkOutcome { label = "final exam", value = Score 85 }
                                  , MkOutcome { label = "midterm1", value = Score 74 }
                                  , MkOutcome { label = "midterm2", value = Score 91 }                                  
                                  , MkOutcome { label = "problemsets"
                                              , value = ListScores [ 95.38461538461539, 0, 87.5, 66.25, 100, 100, 90, 90, 100, 95] }
                                ]                                                                    
                                }
