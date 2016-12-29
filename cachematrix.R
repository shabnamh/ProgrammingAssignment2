## The goal of my functions is to compute the inverse of a matrix and cache its inverse
## to avoid repeated computation and decrease the computation time. One function creates
## a special matrix that cache its inverse while the other function computes the inverse
## of the special matrix returned by the first fucntion or by retriving the inverse if 
## it already exists

## My first function "makeCacheMatrix" takes matrix 'x' as input and creates a special
## matrix that can cache its inverse using lexical scoping in R 

makeCacheMatrix <- function(x=matrix()){
    #set the inverse to null
    Inv <- NULL
    #cache the matrix outside the environment
    set <- function(y){
        x <<- y
        Inv <<- NULL
    }
    #get the matrix from cache
    get <-function (){
        x
    } 
    #solve for the inverse and cache it outside the environment
    setinv <- function(solve){
        Inv <<- solve
    }
    #get the inverse from cache
    getinv <- function(){
        Inv
    }
    #setup the list of matrices to be returned from each function
    list( set=set, get=get, setinv=setinv, getinv=getinv)
    
}


## My second function "cacheSolve" computes the inverse of x,a special matrix returned 
## with the first function. If the inverse already exists,function retrieves it.

cacheSolve <- function (x, ...){
    #get inverse matrix from x
    Inv <- x$getinv()
    
    #if the matrix has been solved and has inverse will return as non-null
    if(!is.null(Inv)){
        message("Getting cached matrix")
        #returns the inverse
        return(Inv)
    }
    #Otherwise the matrix is passed to vector
    data <- x$get()
    #inverse is computed using "solve"
    Inv <- solve(data, ...)
    #cache the matrix
    x$setinv(Inv)
    #return the inverse
    Inv
}
