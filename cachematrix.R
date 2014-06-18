## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
        inverse <- matrix(nrow=nrow(x), ncol=ncol(x))
        set <- function(y) {
                x <<- y
                inverse <<- matrix(nrow=nrow(x), ncol=ncol(x))
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.na(inv[1,1])) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
