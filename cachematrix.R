## The first function, "makeCacheMatrix", creates a special "matrix", which is really a list containing a function to set the matrix,
## invert it, and cache it.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This second function, "cacheSolve", computes the inverse of the special “matrix” returned by the makeCacheMatrix function above. It first checks
## to see if the matrix has already been inverted. If so, it gets it from the cache and skips the computation; otherwise, it inverts it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}