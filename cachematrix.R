## function makeCacheMatrix creates a list to set the value of the matrix, get the value of the matrix, 
## set the inverse of the matrix and get the inverse of the matrix, analogically to the example of
## the mean of the vector.

## function cacheSolve gets the inverse of the matrix, either from the cache, or if the inverse was not yet
## computed, it computes the inverse

## this function creates a special matrix that can handily store its inverse

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
+       set <- function(y) {
+               x <<- y
+               matinv <<- NULL
+       }
+       get <- function() x
+       setinv <- function(solve) matinv <<- solve
+       getinv <- function() matinv
+       list(set = set, get = get,
+            setinv = setinv,
+            getinv = getinv)
}


## this function solves the inverse of the matrix, or gets it from cache if it already has been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getinv()
+       if(!is.null(matinv)) {
+               message("getting cached data")
+               return(matinv)
+       }
+       data <- x$get()
+       matinv <- solve(data, ...)
+       x$setinv(matinv)
+       matinv
}
