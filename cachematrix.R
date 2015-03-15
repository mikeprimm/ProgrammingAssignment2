## Provide a wrapper for invertable matrix() instances, in order to cache results of 
## matrix inversions (which can be very expensive to recalculate).  First function,
## makeCacheMatrix(x), is used to create the needed wrapper.  The second, cacheSolve(x,...),
## is used to request the inverse of the matrix (returning a cached value, if available)

## Create a wrapper for the provided invertable matrix, x, which will allow caching
## of the inverse of the matrix, once requested.  Replacing the matrix's value, via the
## set(y) method for the wrapper, will also invalidate any cached inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Default cached inverse value to null
    inv <- NULL
    # Create set(y) method for wrapper - clear cached inverse while updating matrix value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Create get() method for wrapper - returns matrix value
    get <- function() x
    # Create setinvert() method for wrapper - used to store cacheable inverse of matrix
    setinvert <- function(invert) inv <<- invert
    # Create getinvert() method for wrapper - used to return cached inverse of matrix
    getinvert <- function() inv
    # Return list object, with fields named to correspond to the various methods
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}

# Return solve() value for matrix from wrapper, using cached copy if one is available and
# still valid
cacheSolve <- function(x, ...) {
    # Return the cached inverse, if any
    inv <- x$getinvert()
    # If the cached value is null, compute inverse
    if(is.null(inv)) {
        # Get the original matrix
        data <- x$get()
        # Compute the inverse of the matrix
        inv <- solve(data, ...)
        # Pass the inverse to the wrapper, to allow it to be cached
        x$setinvert(inv)
    }
    else {
        message("getting cached data")
    }
    # Return the inverse
    inv
}
