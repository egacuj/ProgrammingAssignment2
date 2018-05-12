## This function creates a special "matrix" object that can cache its inverse,
## which is really a list containing a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    M_inv <- NULL
    set <- function(y) {
        x <<- y
        M_inv <<- NULL
    }
    get <- function() x
    setM_inv <- function(inverse) M_inv <<- inverse
    getM_inv <- function() M_inv
    list(set = set, get = get,
         setM_inv = setM_inv,
         getM_inv = getM_inv)    
}


## This function returns the inverse of the input matrix 'x', which is assumed to be invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    M_inv <- x$getM_inv()
    if(!is.null(M_inv)) {
        message("getting cached data")
        return(M_inv)
    }
    data <- x$get()
    M_inv <- solve(data, ...)
    x$setM_inv(M_inv)
    M_inv
}
