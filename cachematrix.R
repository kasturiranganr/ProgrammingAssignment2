## The makeCacheMatrix function creates a special matrix
## object that can cache it's inverse while the cacheSolve
## function computes the inverse of the matrix

## makeCacheMatrix function
## This function sets a matrix, gets a matrix, sets the inverse
## of a matrix and gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
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


## cacheSolve function
## This function first gets the inverse of a matrix passed into it
## by calling the cache. If it cannot find it it just gets the matrix
## object and computes the inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}
