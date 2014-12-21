## The functions in this module goal is to aid the step of carrying out an inversion
## of a matrix repeatly which can be an expensive operation for a large matrix by caching
## the creation step of the object - so expensive computation is carried out only once.
## Subsequent calls to get matrix inverse are fast.

## The first function, makeCacheMatrix creates a special "vector", which is  a list containing a function to  
##  set the value of the matrix 
##  get the value of the matrix 
##  set the value of the inverse of matrix by solve 
##  get the value of the inverse of matrix by solve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve<- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #message("start")
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
