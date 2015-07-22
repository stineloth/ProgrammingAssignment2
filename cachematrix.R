## These functions (makeCacheMatrix and cacheSolve) together create a special matrix and then provide the inverse of the matrix.
## Since inverting a matrix can be costly, these functions cache the inverse so that it can be used again, rather than
## computing it repeatedly.

## makeCacheMatrix creates a list of functions 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                     ## Initialize inv to null
    set <- function(y) {                            ## Set function
      x <<- y                                 
      inv <<- NULL
    }
    get <- function() x                             ## Get function
    setinverse <- function(inverse) inv <<- inverse ## Set inverse
    getinverse <- function() inv                    ## Get inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## cacheSolve determines if the inverse has already been computed, and if it has it returns the inverse
## Otherwise it computes the inverse
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                             
  if(!is.null(inv)) {                               ## Determine if cache already exists
    message("getting cached data")            
    return(inv)                                     ## If inverse exists, just return it
  }
  data <- x$get()                                   ## Otherwise, get matrix
  inv <- solve(data, ...)                           ## Compute inverse
  x$setinverse(inv)
  inv                                               ## Return inverse
}
