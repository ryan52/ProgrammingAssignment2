## These functions implement an object in R representing a matrix with the
## ability to cache its own inverse when using the cacheSolve function.

## makeCacheMatrix makes an R object to cache the inverse for a given matrix

makeCacheMatrix <- function(x = matrix()) {
        # initially we do not know the inverse
        i <- NULL

        # define the functions this object implements
        set <- function(y) {
                x <<- y
                # must reset cached inverse when x changes
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i

        # return the object as a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve solves the inverse for the matrix object, unless it is cached

cacheSolve <- function(x, ...) {
        # if it is cached, then just return it
        i <- x$getinverse()
        if(!is.null(i)) {
                # for debugging: message("getting cached data")
                return(i)
        }

        # otherwise, solve and cache it for future use
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)

        ## Return a matrix that is the inverse of 'x'
        i
}
