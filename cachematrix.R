## Store a matrix and its inverse. If an inverse is not present it is calculated

## Function to store a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) I <<- Inverse
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function to calculate Inverse of matrix or retrieve one already stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data)
        x$setInverse(I)
        I
}
