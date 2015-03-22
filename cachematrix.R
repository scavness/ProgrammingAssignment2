## The following functions creat a special "matrix" that can cache its inverse
## then compute the inverse if it has not already been cached, and return the result

## Takes in a matrix, cache's its inverse if it has been calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = get, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Returns the inverse of the matrix given, uses the cached version if
## available rather than re-computing the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
