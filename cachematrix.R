## These functions have the special ability to create a matrix and then
## cache its inverse

## creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ##where i is the inverse matrix
        setMatrix <- function(y) {  #where y is already a matrix 
                x <<- y
                i <<- NULL
        }
        getMatrix <- function( ) {x}    ##where x is already a matrix
        setINV <- function (inv) {i <<- inv}
        getINV <- function() {i}
        list(setMatrix = setMatrix, getMatrix = getMatrix, setINV = setINV, getINV = getINV)
}

## caches the inverse of the matrix created above if the matrix has changed

cacheSolve <- function(x, ...) {
        i <- x$getINV()
        if(!is.null(i)) {
                print("getting cached data")
                i
        }
        m <- x$getMatrix()
        i <- solve(m,...)
        x$setINV(i)
        i
        
}
