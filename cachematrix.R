## L <- matrix()
## cL <- makeCacheMatrix(L)
## cacheSolve(cL) # - returns solve(matrix) either from cach w/warning
##    or just calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NA
  set <- function(y) {
    x <<- y
    y <<- NA
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## x$getInverse() returns NA if makeCacheMatrix wasnt run before

cacheSolve <- function(x, ...) {
  ## check if x$getInverse() already stored then return
  m <- x$getInverse()
  if (!is.na(m)) {
    message("getting cached data")
    return(m)
  }
  ## or this calculates it by running solve(x$get()) and return m
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
