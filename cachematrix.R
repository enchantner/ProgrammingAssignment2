## Functions representing caching implementation of
## calculating inverse of a matrix

## Returns an object (list), which is also cacheable
## matrix with getter/setter for both data and cache
## values

makeCacheMatrix <- function(x = matrix()) {
  # checking the input
  if (!is.matrix(x)) stop("This argument doesn't look like a matrix")
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  # setter and getter for an inverse
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates inverse of a matrix returned by makeCacheMatrix
## or returns cached value if it's present

cacheSolve <- function(inversible, ...) {
  inv <- inversible$getinv()
  # checking if inverce value is already in cache
  if(!is.null(inv)) {
    message("Getting cached inverse value")
    return(inv)
  }
  # calculating inverse if cache is empty
  data <- inversible$get()
  inv <- solve(a=data, ...)
  inversible$setinv(inv)
  inv
}
