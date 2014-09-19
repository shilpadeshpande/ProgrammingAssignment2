## These functions are especially useful when we have to calculate the 
## inverse of the matrix repeatedly. If the matrix has been unchanged, 
## one can cache the inverse and return the same, saving extraneous computing.

## This function creates a Matrix object. This function expects that the input matrix is invertible. 
## If the input matrix is not invertible, the error will be returned.
## This object has utility methods like set, get, setinverse and getinverse. 
## If the matrix has been reset, inverse is calculated again.
makeCacheMatrix <- function(x = matrix()) {
        if(nrow(x) != ncol(x)) {
                stop('The input matrix has be a square matrix. For example, 2x2 or 3x3 matrix.')
        }
        if(det(x) == 0) {
                stop('The input matrix cannot be singular.')
        }
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the Matrix object. The input
## to this function is a Matrix object created with function makeCacheMatrix.
## If the inverse is already been calculated then the same is returned.
## If the internal matrix has changed then newer inverse of the matrix is returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
