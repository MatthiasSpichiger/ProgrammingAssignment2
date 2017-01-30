## The function "MakeCacheMatrix()" should create a matrix-like Object that can cache the inverse of a matrix.
## The function "cacheSolve()" is supposed to compute the inverse of a matrix. If the inverse has already been calculated, it shoud use the
## inverse that is stored in the cache.

## This function is supposed to set the matrix, get the matrix, set the inverse and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                 x <<- y
                inv <<- NULL
       }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)                
}


## This function should compute the inverse that was returned by the aforementioned function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        } 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        
        return(inv)        
}
