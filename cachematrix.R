## set fucntion - sets the value of the matrix
## get function - gets the value of the matrix
## setsolve function - creates (or sets) the inverse of the original matrix
## getsolve function - returns (or gets) the inverse of the original matrix

## makeCacheMatrix creates x which is a blank matrix
## makeCacheMatrix also contains the functions (and a list of) set, get, setsolve, getsolve

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


## makeCacheSolve checks to see if makeCacheMatrix has create of the matrix's inverse
## if it has it returns the cached matrix
## If makeCacheSolve has not created the inverse of the matrix,it creates of the inverted matrix 

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

