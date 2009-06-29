# get or set the logfile
ectdLogFile <- function( file ){
  current <- get("logfile", env = .ectdEnv)
  if( !missing(file) ) assign("logfile", file, env = .ectdEnv )
  invisible(current)
}

# get or set the verbose
ectdVerbose <- function(verbose){
  current <- get("verbose", env = .ectdEnv)
  if( !missing(verbose) ) assign("verbose", verbose, env = .ectdEnv )
  invisible(current)  
}

# get or set the dateFormat
ectdDateFormat <- function(format){
  current <- get("dateFormat", env = .ectdEnv)
  if( !missing(format) ) assign("dateFormat", format, env = .ectdEnv )
  invisible(current)     
}

  .ectdEnv <- new.env( )
  assign( "logfile", "ectd.log", env = .ectdEnv )
  assign( "verbose", TRUE, env = .ectdEnv )
  assign( "dateFormat","%Y-%m-%d %H:%M:%OS4" , env = .ectdEnv )
  
  
.onAttach <- function(libname, pkgname ){
  source( file.path( .path.package("MSToolkit"), "ECTD.ini" )  )
  copyright <- readLines(system.file("COPYRIGHT", package = "MSToolkit" ) )
  copyright <- gsub( "\\$version", 
      sprintf( "%-12s", packageDescription("MSToolkit", fields = "Version") ), 
      copyright
  )
  cat( copyright, sep = "\n" )

 if( "RUnit" %in% search() || "RUnit" %in% .packages(all = TRUE)){ 
 cat("#########################################################\n")
cat("#                                          Unit tests    #\n") 
cat("# > mstoolkitUnitTests( )                                #\n")
cat("##########################################################\n")
 }
 
 if( .checkGridAvailable() ){
cat("################################################################################\n")
cat("# You are running the package from a grid node. To take advantage of it, use   #\n")
cat("# the grid argument in ?analyzeData                                            #\n")
cat("################################################################################\n")
 }
}

.canUsePerl <- function(){
   .Platform$OS.type != "windows" && length( system("which perl", ignore.stderr = TRUE, intern = TRUE) ) > 0
}

