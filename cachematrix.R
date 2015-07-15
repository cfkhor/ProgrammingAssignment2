## The following is a pair of functions that cache and compute the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
inverse <- NULL
set <- function(x) {
mtx <<- x;
inverse <<- NULL;
}
get <- function() return(mtx);
setinverse <- function(inv) inverse <<- inv;
getinverse <- function() return(inverse);
return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}





## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
inverse <- mtx$getinverse()
if(!is.null(inverse)) {
message("Getting cached data...")
return(inverse)
}


data <- mtx$get()
invserse <- solve(data, ...)
mtx$setinverse(inverse)
return(inverse)
}
