# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
     
     inv_x <- NULL
     
     set <- function(y) {
          x <<- y
          inv_x <<- NULL
     }
     
     get <- function() x
     
     setinverse<- function(inverse) inv_x <<-inverse
     
     getinverse <- function() inv_x
     
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


# The function cacheSolve() returns the inverse of a matrix created 
# with the makeCacheMatrix function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
     
     # Return a cached matrix that is the inverse of 'x'
     inv_x <- x$getinverse()
     
     # Cheking if inv_x is not empty (was in cashe)
     if (!is.null(inv_x)) {
          
          message("getting cached inverse matrix")
          return(inv_x)
          
     } else {
          
          # inv_x wasn't in cahce so we will compute it, cache it and return it.
          inv_x <- solve(x$get())
          x$setinverse(inv_x)
          return(inv_x)
          
     }
}
