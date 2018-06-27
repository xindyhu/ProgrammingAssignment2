## These two functions will cache the inverse of a matrix such that inverse of a matrix will be stored rather
## than being computed repeatedly

## This function creates a special "matrix" object that can cache its inverse, which is really a list containing 
## a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
 
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

## This following function calculates the mean of the special "matrix" created with the above function.
## First it checks to see if the inverse has already been calcualted. If so, it gets the inverse from the cache
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the 
## setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
