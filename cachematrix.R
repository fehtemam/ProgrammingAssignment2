## This script includes two functions: makeCacheMatrix & cacheSolve

## makeCacheMatrix takes a matrix as an input and gives the inverse of it as the output

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(t) {
            x <<- t
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve takes a matrix and uses lexical scoping to see if its inverse is calculated or not. 
## If it is not, then it calculates it from scratch. Otherwise it just returns the value already available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
