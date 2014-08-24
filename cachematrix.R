# cachematrix.R
#
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly.
# Use the 'makeCacheMatrix' function to create a "special vector"
# from the original matrix you want to inverse.  Use the "vector" returned
# by 'makeCacheMatrix' and call the 'cacheSolve' function with it.
# The first time, 'cacheMatrix' will go through the actual computation
# necessary to compute the inverse matrix and will cache it before
# returning it.
# The next times you call 'cacheMatrix' with the same instance of the
# "special vector", the cached version of the inversed matrix will get returned.
# ----------------------------------------------------------------------

# This function creates a special "vector " which is really a list
# containing functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inversed matrix
# 4. get the value of the inversed matrix
#
makeCacheMatrix <- function(x = matrix()) {
   ## Return a "special vector" that adds the caching functionality
   cachedInversed <- NULL
   set <- function(y) {
      x <<- y
      cachedInversed <<- NULL
   }
   get <- function() x
   setinverse <- function(inversed) cachedInversed <<- inversed
   getinverse <- function() cachedInversed
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The following function calculates the inverse of the vector carried by
# the special "matrix" created with the 'makeCacheMatrix' function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of
# the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   cachedInversed <- x$getinverse()
   if(!is.null(cachedInversed)) {
      message('getting cached data')
      return(cachedInversed)
   }
   data <- x$get()
   cachedInversed <- solve(data, ...)
   x$setinverse(cachedInversed)
   cachedInversed
}
