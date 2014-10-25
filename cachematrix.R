## Solves and caches inverse of matrix.
## Usage:
##  > cache_matrix <- makeCacheMatrix(c(11,12,21,22), ncol=2, nrow=2)
##  > inverse <- cacheSolve(cache_matrix)

## Attaches getters/setters for both the object 
## and the inverse property.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  ## This function resets the cache (inverse <<- NULL) when the
  ## value of the original matrix (x) changes.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  ## Getter for self.
  get <- function() x

  ## Sets the inverse 'property' to inv
  setinverse <- function(inv) inverse <<- inv

  ## Gets the inverse 'property'
  getinverse <- function() inverse

  ## List
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       solve=solve)
}

## This function leverages the $inverse property
## to avoid expensive calculations in solve(x)
cacheSolve <- function(x, ...) {
  ## Get the inverse (matrix or NULL)
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    ## Cache hit, return the cached value.
    return(inverse)
  }
  ## If we're here, it was a miss. Get the actual result
  ## with solve()
  inverse <- solve(x$get())

  ## Set the inverse to the new value with the setter.
  x$setinverse(inverse)
  inverse
}