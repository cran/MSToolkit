
# is there at least one element of x that matches the regex rx
`%~%` <- function(x, rx){
  any( regexpr(rx, x)  > 0 ) 
}

# TRUE if no element of x matches the regex rx
`%!~%` <- function(x, rx){
  out <- any( rxout <- regexpr(rx, x) == -1 )
  assign( "..nm", which(rxout), parent.frame() )
  out
}

# negation of %in%
`%!in%`    <- function(...) !`%in%`(...)

# wrap all around %in%
`%allin%`  <- function(...) all( `%in%`(...) )
`%!allin%` <- function(...) !all( `%in%`(...) )

# wrap any around %in%
`%anyin%`  <- function(...) any( `%in%`(...) )
`%!anyin%` <- function(...) !any( `%in%`(...) )

`%in~%` <- function(x, y){
  x %in% strsplit(y, '')[[1]]
}
`%!in~%`<- function(...) !`%in~%`(...)
`%allin~%` <- function(...) all( `%in~%`(...) )
`%!allin~%` <- function(...) !all( `%in~%`(...) )

## operator to check that two objects have the same length
`%l%` <- function(x,y){ 
  length(x) == length(y) 
}
`%!l%` <- function(...) !`%l%`(...) 

# string concatenation
`%.%` <- function(s1, s2) {
  paste(s1, s2, sep = "") 
}                                  

# .. with a newline
`%.n%` <- function(s1, s2) {
  paste(s1, s2, sep = "\n", collapse = "\n") 
}
# .. with a new line and a tabulation
`%.nt%` <- `%.tn%` <- function(s1, s2) {
  paste(s1, s2, sep = "\n\t") 
}

# .. with a space
`%.s%` <- function(s1, s2) {
  paste(s1, s2, sep = " ", collapse = "\n")                       
}

# .. remove a pattern from a string
`%-~%` <- function(txt, rx){
  gsub( rx, '', txt )                
}     

# is that oject of that class
`%of%` <- function(e1, e2){
  inherits( e1, e2 ) 
}

`%!of%` <- function(...) !`%of%`(...) 

# assign value to the value of name
`%<-%` <- function(name, value){
  assign( name, value, parent.frame() )
}

# exclusion
`%without%` <- `%wo%` <- `%w/o%` <- function(x, y){
  x[!x %in% y ]
}
  

# perl-like statement modifiers
`%if%` <- function( expr, condition ){
  if( condition ) eval(expr, parent.frame() )
}
`%unless%` <- function( expr, condition ){
  if( !condition ) eval(expr, parent.frame() )
}


