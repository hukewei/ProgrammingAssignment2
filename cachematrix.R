## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function will build and returns a special object
## that contains 4 methods: get, set, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a parameter x created from makeCachedMatrix
## and returns the inverse of the x
## If the inverse has already been computed before, it returns the cached results

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
