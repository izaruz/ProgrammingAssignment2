## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  set <- function(y) {
    ## The simbol <<- set the value in the enviroment
    ## Is a superassignment
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x
  setinverseM <- function(inverse) inverseM <<- inverse
  getinverseM <- function() inverseM
  list(set = set, get = get,
       setinverseM = setinverseM,
       getinverseM = getinverseM)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {
  inverseM <- x$getinverseM()
  if(!is.null(inverseM)) {
    message("getting cached data")
    return(inverseM)
  }
  mat.data <- x$get()
  inverseM <- solve(mat.data, ...)
  x$setinverseM(inverseM)
  return(inverseM)
}

## Isaac Villatoro - izaruz@yahoo.com
