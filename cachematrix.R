## This function creates a list to 1) set the value of the matrix, 2) get the value of the matrix,
## 3) set the value of the inverse, and 4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) ## creation of the list
}

## This function relies on the previous function to check if the inverse of a matrix has already been
## calculated. If so, it retrieves in from cache and skips the computation. Else, it calculates it.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {          ## checking if the inverse is in cache
    message("getting cached data")
    return(m)                ## prints the inverse directly and ends the function
  }
  data <- x$get()
  m <- solve(data, ...)      ## computes the inverse when is not in cache.
  x$setsolve(m)
  m
}
