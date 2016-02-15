## This source file contins two functions: makeCacheMatrix and CacheSolve
## that can be used to construct a special matrix that can cache both a 
## matrix and its inverse. If the matrix is changed, the inverse cache 
## is reset to an empty matrix

## The matrix provided must always be a square invertible matrix.

## The following function creates a special "matrix" object that can cache a
## matrix and its inverse with four functions: (1) set the value of the matrix,
## (2) get the value of the matrix (3) set the value the inverse and (4) get 
## the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- matrix()
     set <- function(y) {
          x <<- y
          inv <<- matrix()
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
     
}


## The following function returns the inverse of the special "matrix" created 
## by makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve will return the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     ## Test to see if the inverse matrix is not empty
     if(!identical(matrix(),inv)) {
          message("getting cached data")
          return(inv)
     }
     ## get the matrix and compute inverse, set and return inverse matrix
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
