## creates a special "matrix" object that can cache its inverse.
## Than computes the inverse of the special "matrix" 

## This function creates a special "matrix" which is a list containing a function 
## to set and get matrix and its invers

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvers <- function(inverz) m <<- inverz
  getInvers <- function() m
  list(set = set, get = get,
       setInvers = setInvers,
       getInvers = getInvers)
  
}


## Cache the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvers()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvers(m)
  m
}