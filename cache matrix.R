# Function that set the matrix and the inverse in an environment
# X is an invertible matrix
# Pass the result of a makeCacheMatrix call to cacheSolve 

makeCacheMatrix <- function(x = matrix()) 
  {
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

# Compute and cache the inverse of a matrix
# x is the result of a previous makeCacheMatrix call
# (...) is used as additional arguments to pass to solve function

cacheSolve <- function(x, ...) 
  {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
