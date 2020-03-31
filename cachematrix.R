## These functions cache the inverse of a given matrix.

## This function inputs a square matrix and stores its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(ma){
          x <<- ma
          inv <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) inv <<- solve
     getsolve <- function() inv 
     
     list(set = set, get = get, 
          setsolve = setsolve,
          getsolve = getsolve)
     
}


## This function retrieves the stored inverse matrix from makeCacheMatrix and caches it. 
##If already cached, it gets it from a previous cacheSolve call. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getsolve()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setsolve(inv)
     inv
     
}
