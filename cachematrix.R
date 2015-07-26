## This function creates a matrix and calculates the inverse

##This function creates a matrix and does the following
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y         
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##the following function calculates the inverse of the matrix
##created with the above function, but it first checks if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the matrix and sets the value of the
##inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting solved matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

