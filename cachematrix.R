## Coursera Course R Programming
## Programming Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix, since it's a potentially time-consuming computation

## This function creates a special "matrix" object that can cache its inverse
## It contains a function to 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) invers <<- solve
  getsolve <- function() invers
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {    ## Return a matrix that is the inverse of 'x'
  invers <- x$getsolve()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  invers <- solve(data, ...)
  x$setsolve(invers)
  invers
}
