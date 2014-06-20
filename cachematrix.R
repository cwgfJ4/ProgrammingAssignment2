## the two functions makeCacheMatrix and cacheSolve set up und use a type
## of matrix which internally stores its inverse, so that it can be 
## recalled without (possibly lengthy) computation

## makeCacheMatrix: initialitze a matrix including cache for inverse
## and functions set, get, setinv, getinv to operate on the matrix

makeCacheMatrix <- function(x = numeric()) {

        ## first, initialize inverse with same number of rows and columns 
        inverse <- matrix(nrow=nrow(x), ncol=ncol(x))

        ## define a set function to set the value of the matrix
        set <- function(y) {

                x <<- y
                inverse <<- matrix(nrow=nrow(x), ncol=ncol(x))

                ## to assign x and inverse (in parent environment),
                ## <<- is used instead of <- (which would not have any
                ## effect outside the set function
        }

        ## define a get function that returns the matrix
        get <- function() x

        ## define a setinv function that sets the inverse
        setinv <- function(inv) inverse <<- inv
        ## use <<- to assign inverse as in set function

        ## define a getinv function that returns the inverse
        getinv <- function() inverse

        ## Return a list of four functions
        ## set: set the value of the matrix, initialize cache (for inverse)
        ## get: get the value of the matrix
        ## setinv: set the value of the inverse
        ## getinv: get the value if the inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: calculate the inverse of a matrix x defined by makeCacheMatrix


cacheSolve <- function(x, ...) {

        ## if the inverse was calculated before, 
        ## return the cached result without a computation

        ## read the cached inverse
        inv <- x$getinv()

        ## was the inverse calculated before (upper left element not NA)?
        if(!is.na(inv[1,1])) {
 
                ## yes: simply return the cached inverse without computation
                message("getting cached data")
                return(inv)
        }

        ## if not, get the matrix data and use the solve function
        data <- x$get()
        inv <- solve(data, ...)

        # store the inverse in cache for the next cacheSolve call
        x$setinv(inv)

        # return the newly calculated inverse
        inv
}