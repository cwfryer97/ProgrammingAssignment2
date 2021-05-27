## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## the function creates four functions: set, get, setinv, getinv
## set stores the matrix x in the function environment and sets up a NULL 
##   place-holder for the inverse inv
## get retrieves the matrix x
## setinv stores the inverse inv in the function environment
## getinv retrieves the inverse inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## the function returns the inverse of a matrix which is stored by a call to 
## makeCacheMatrix
## it gets inv from the environment of the functions in x
## if inv is not NULL, then the inverse has already been calculated, so the function
##   says so and returns the inverse 
## if inv is NULL, then the function calculates the inverse using solve, stores it
##   in the environment of the functions in x so that it can be used next time, and
##   returns the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
