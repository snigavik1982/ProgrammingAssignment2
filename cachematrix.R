## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.na(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
