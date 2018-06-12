## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # Local variable in this function
    im <- NULL
    # Initialize matrix
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    # return x
    get <- function() x
    # set inverse matrix
    setinverse <- function(inverse) im <<- inverse
    # return inverse matrix
    getinverse <- function() im
    # function list
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # get inverse matrix
    im <- x$getinverse()
    if(!is.null(im)) {
        # im is not null, that means im has been caculated
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    # solve
    im <- solve(data, ...)
    # cache
    x$setinverse(im)
    # return the inverse matrix
    im
}
