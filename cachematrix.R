## The first function makeCacheMatrix takes a matrix as and argument and returns 
## a list of functions, get, set, getInv and setInv 
## set caches the value of x and set the inverse to Null
## get retrives the x value
## setInv calculate the inverse of the matrix and cache it as I
## getInv retrive the value of I 

makeCacheMatrix <- function(x = matrix()) {

        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinv <- function(solve) I <<- solve
        getinv <- function() I
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)



}


## cacheSolve takes in list from the makeCacheMatrix function
## checks if the inverse has been calculated already
## if yes, it retrives its value and prints it
## else, it calculate the inverse and caches it as I

cacheSolve <- function(x, ...) {
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }

        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I

}
