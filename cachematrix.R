## The goal of my functions is to compute the inverse of a matrix and cache its inverse
## to avoid repeated computation and decrease the computation time. One function creates
## a special matrix that cache its inverse while the other function computes the inverse
## of the special matrix returned by the first fucntion or by retriving the inverse if 
## it already exists

## My first function "makeCacheMatrix" takes matrix 'x' as input and creates a special
## matrix that can cache its inverse using lexical scoping in R 

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y){
        x <<- y
        Inv <<- NULL
     }
    get <-function (){
        x
     } 
    setinv <- function(solve){
       Inv <<- solve
     }
    getinv <- function(){
       Inv
     }
    list( set=set, get=get, setinv=setinv, getinv=getinv)
}


## My second function "cacheSolve" computes the inverse of x,a special matrix returned 
## with the first function. If the inverse already exists,function retrieves it.

cacheSolve <- function(x, ...) {
   Inv <- x$getinv()
   if(!is.null(Inv)){
      message("Getting cached data")
      return(Inv)
   }
   data <- x$get()
   Inv <- solve(data, ...)
   x$setinv(Inv)
   Inv        
}
