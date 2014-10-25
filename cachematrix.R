## Solves and caches inverse of matrix.
## Usage:
##  > cache_matrix <- makeCacheMatrix(c(11,12,21,22), ncol=2, nrow=2)
##  > inverse <- cacheSolve(cache_matrix)

## Attaches getters/setters for both the object 
## and the inverse property.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## This function resets the cac
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       solve=solve)
}

## This function leverages the $inverse property
## to avoid expensive calculations in solve(x)
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  message("calculating inverse...")
  inverse <- solve(x$get())
  message("setting cache...")
  x$setinverse(inverse)
  inverse
}