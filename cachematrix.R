## cachematrix.R
## By hanish

## A pair of functions that cache the Inverse of a matrix
##  This functions creates a special "matrix" object that can cache its inverse
##<<- operator is used to assign a value to an object in an environment that is different from current environment

makeCacheMatrix <- function(x = matrix())  {
     inv <- NULL
     set <- function(y){
            x <<- y
            inv <<- NULL
        }
     get <- function() x
     setInverse <- function(solveMatrix) inv <<- solveMatrix
     getInverse <- function() inv
     list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getInverse()
     if(!is.null(inv)){
           message("getting cached data")
           return(inv)
           }
     data <- x$get()
     inv <- solve(data)
     x$setInverse(inv)
     inv
             }

