## Matrix inverse calculation that supports caching of results

## Prepares a representation of x that can be passed to cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  
  # Simple object system
  # Data is cached in the clojure
  # Methods are passed in the list
  list( 
        # Resets matrix to new value, this resets the cache
        set = function(new_x) {
          x<<-new_x
          cache<<-NULL
        },
        get = function() x,
        getinv = function() cache,
        setinv = function(inv) cache <<- inv
  )
  
}


## Perform matrix inversion for x, where x is an object returned by makeCacheMatrix, and returns inverse matrix
## This functions perform memoization of the results in x
## Thus, inverse is calculated only once for each matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  # We found cached data, so let's return the cache
  if (!is.null(inv)) {
    message("getting cached data")
    inv
  }
  
  # Nothing is found so we will need to really do the inversion
  m <- x$get();
  
  # A*X=1
  inv <- solve(m, rep(1, ncol(m)), ...)
  
  # Store the inverse in the cache
  x$set(m)
  x$setinv(inv)
  
  inv
  
}
