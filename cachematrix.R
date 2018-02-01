## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Make cache of inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Switch out m in source code with inv so that an inversion is performed.
  get <- function() x
  setinv <- function(solveMatrix) inv <<- solveMatrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #If there is no data in the original data source, then solve the data as an inversion. 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
