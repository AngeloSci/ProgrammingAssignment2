## The two functions address efficiency issues when calculating the
# inverse of a matrix. makeCacheMatrix creates a special matrix
# that can cache the inverse of the matrix itself. cacheSolve computes
# the inverse of a matrix only if it has not been compute yet, otherwise it 
# returns the cached inverse matrix.


## makeCacheMatrix takes a matrix as input and returns a matrix that
# can cache the inverse of itself. It is basically a list of functions
# to: get the matrix; set the matrix; get the inverse matrix; set the
# inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinv <- function(inv_matrix) inv <<- inv_matrix
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of a matrix only it has not been computed 
# yet. If it has been computed and cached, it will return the cached inverse
# matrix. cacheSolve takes a matrix created with makeCacheMatrix as input.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting chached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
    
}
