## Matrix Inversion Processor
## This program will produce an inverse of a matrix
## if the inverse has already been found in cache
## it will retrieve and return the cached matrix

## makeCacheMatrix will provide the mechanism to cache 
## the inverse of the matrix once it is found

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(solve) m <<- solve
        getinvmatrix <- function() m
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


## cacheSolve will calculate the inverse matrix 
## and save the result into the provided function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinvmatrix(m)
        m
}
