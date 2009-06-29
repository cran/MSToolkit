if( !exists("unitTestPath")) unitTestPath <- "."
compileSummary.datapath <- file.path( unitTestPath , "data", "compileSummary" )
compileSummary2.datapath <- file.path( unitTestPath , "data", "compileSummary2" )
cat("compileSummary.datapath:", compileSummary.datapath, "\n" )
cat("compileSummary2.datapath:", compileSummary.datapath, "\n" )

test.compileSummary.failure <- function(){

  checkException( compileSummary("frew"), 
    msg = "Micro or Macro" )
    
  dir.create( tf <- tempfile() )
  checkException(compileSummary("Micro", workingPath = tf ), 
     msg = "no MicroEvaluation directory" )
  checkException(compileSummary("Macro", workingPath = tf ), 
     msg = "no MacroEvaluation directory" )
  dir.create( file.path(tf, "MicroEvaluation") )
  dir.create( file.path(tf, "MacroEvaluation") )
  checkException(compileSummary("Micro", workingPath = tf ), 
     msg = "no csv files in MicroEvaluation directory" )
  checkException(compileSummary("Macro", workingPath = tf ), 
     msg = "no csv files in MacroEvaluation directory" )
  
  unlink(tf, recursive = TRUE)  
  
}

### RUNit test for test.compileSummary.perl omitted
### This functionality not required in current version
### MKS - 08 Sept 09

test.compileSummary.R <- function(){  
  
  ## copy files accross
  dir.create( cpdir <- file.path(tempdir(), "compileSummary.R" ) )
  dir.create( file.path(cpdir, "MacroEvaluation" ) )
  dir.create( file.path(cpdir, "MicroEvaluation" ) )
  for( i in 1:5){
    file.copy( file.path(compileSummary.datapath, "MicroEvaluation",  sprintf("micro%04d.csv", i)) , 
      file.path(cpdir, "MicroEvaluation"    ) )
    file.copy( file.path(compileSummary.datapath, "MacroEvaluation",  sprintf("macro%04d.csv", i)) , 
      file.path(cpdir, "MacroEvaluation"    ) )
  }
  
  compileSummary( "Micro", workingPath = cpdir, tryPerl = FALSE )
  compileSummary( "Macro", workingPath = cpdir, tryPerl = FALSE )
  expectedMicroData <- read.csv( file.path(compileSummary.datapath, "microSummary.csv") )
  newMicroData <- read.csv( file.path(compileSummary.datapath, "microSummary.csv") )
  checkEquals(expectedMicroData, newMicroData, 
    msg = "compile micro R")
  expectedMacroData <- read.csv( file.path(compileSummary.datapath, "macroSummary.csv") )
  newMacroData <- read.csv( file.path(compileSummary.datapath, "macroSummary.csv") )
  checkEquals(expectedMicroData, newMicroData, 
    msg = "compile macro R")
  
  unlink(cpdir, recursive = TRUE)
  
}

test.compileSummary.missing <- function(){  
  ## copy files accross
  dir.create( cpdir <- file.path(tempdir(), "compileSummary.missing" ) )
  dir.create( file.path(cpdir, "MacroEvaluation" ) )
  dir.create( file.path(cpdir, "MicroEvaluation" ) )
  for( i in 1:5){
    file.copy( file.path(compileSummary.datapath, "MicroEvaluation",  sprintf("micro%04d.csv", i)) , 
      file.path(cpdir, "MicroEvaluation"    ) )
    file.copy( file.path(compileSummary.datapath, "MacroEvaluation",  sprintf("macro%04d.csv", i)) , 
      file.path(cpdir, "MacroEvaluation"    ) )
  }

  checkException( compileSummary( "Micro", replicates = 1:10, workingPath = cpdir ), 
    msg = "not compile when missing files (micro)" )
  checkException( compileSummary( "Macro", replicates = 1:10, workingPath = cpdir ), 
    msg = "not compile when missing files (macro)" )

    
  
  unlink(cpdir, recursive = TRUE)
  
}

test.compileSummary.holes.perl <- function(){  
  ## copy files accross
  dir.create( cpdir <- file.path(tempdir(), "compileSummary.missing" ) )
  dir.create( file.path(cpdir, "MacroEvaluation" ) )
  dir.create( file.path(cpdir, "MicroEvaluation" ) )
  for( i in 1:5){
    file.copy( file.path(compileSummary2.datapath, "MicroEvaluation",  sprintf("micro%04d.csv", i)) , 
      file.path(cpdir, "MicroEvaluation"    ) )
    file.copy( file.path(compileSummary2.datapath, "MacroEvaluation",  sprintf("macro%04d.csv", i)) , 
      file.path(cpdir, "MacroEvaluation"    ) )
  }

  compileSummary( "Micro", workingPath = cpdir )
  microSum <- read.csv( file.path(cpdir, "microSummary.csv") )
  checkTrue( "DUMMY" %in% names( microSum ),  
    msg = "compile with not same variables" )
  checkTrue( all( is.na( microSum$DUMMY[ microSum$Replicates != 2]  ) ), 
    msg = "NA when trying to compile with not same variables (micro)")
  checkTrue( all( !is.na( microSum$DUMMY[ microSum$Replicates == 2]  ) ), 
    msg = "NA when trying to compile with not same variables (micro,2)")
    
  compileSummary( "Macro", workingPath = cpdir ) 
  macroSum <- read.csv( file.path( cpdir, "macroSummary.csv") )
  checkTrue( "SOMETHING" %in% names( macroSum ),  
    msg = "compile with not same variables" )
  checkTrue( all( is.na( macroSum$SOMETHING[ macroSum$Replicates != 3]  ) ), 
    msg = "NA when trying to compile with not same variables (macro)")
  checkTrue( all( !is.na( macroSum$SOMETHING[ macroSum$Replicates == 3]  ) ), 
    msg = "NA when trying to compile with not same variables (macro,2)")
  
   unlink(cpdir, recursive = TRUE)

    
}
test.compileSummary.holes.R <- function(){  
  ## copy files accross
  dir.create( cpdir <- file.path(tempdir(), "compileSummary.missing" ) )
  dir.create( file.path(cpdir, "MacroEvaluation" ) )
  dir.create( file.path(cpdir, "MicroEvaluation" ) )
  for( i in 1:5){
    file.copy( file.path(compileSummary2.datapath, "MicroEvaluation",  sprintf("micro%04d.csv", i)) , 
      file.path(cpdir, "MicroEvaluation"    ) )
    file.copy( file.path(compileSummary2.datapath, "MacroEvaluation",  sprintf("macro%04d.csv", i)) , 
      file.path(cpdir, "MacroEvaluation"    ) )
  }

  compileSummary( "Micro", tryPerl = FALSE, workingPath = cpdir )
  microSum <- read.csv( file.path( cpdir, "microSummary.csv") )
  checkTrue( "DUMMY" %in% names( microSum ),  
    msg = "compile with not same variables" )
  checkTrue( all( is.na( microSum$DUMMY[ microSum$Replicates != 2]  ) ), 
    msg = "NA when trying to compile with not same variables (micro)")
  checkTrue( all( !is.na( microSum$DUMMY[ microSum$Replicates == 2]  ) ), 
    msg = "NA when trying to compile with not same variables (micro,2)")
    
  compileSummary( "Macro", tryPerl = FALSE, workingPath = cpdir ) 
  macroSum <- read.csv( file.path( cpdir, "macroSummary.csv") )
  checkTrue( "SOMETHING" %in% names( macroSum ),  
    msg = "compile with not same variables" )
  checkTrue( all( is.na( macroSum$SOMETHING[ macroSum$Replicates != 3]  ) ), 
    msg = "NA when trying to compile with not same variables (macro)")
  checkTrue( all( !is.na( macroSum$SOMETHING[ macroSum$Replicates == 3]  ) ), 
    msg = "NA when trying to compile with not same variables (macro,2)")
    
  unlink(cpdir, recursive = TRUE)
  
}
