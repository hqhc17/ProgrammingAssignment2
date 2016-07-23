## The two functions below ensure that the inverse of a matrix is computed
## only once and if it is needed more than once, it is retrieved from a cache.

## makeCacheMatrix returns a list of four functions while also caching 

makeCacheMatrix <- function(x = matrix()) {
	## Initialize the Inverse to NULL
    Inverse <- NULL

	## Setter to assign variable y to x in the PARENT environment
	## and NULL to the Inverse in the PARENT environment. Thus, if x is
	## reset through a call to set function, the inverse is reset to NULL
	## and cacheSolve function is forced to calculate the inverse of x.
    set <- function(y) {
        x <<- y
        Inverse <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) Inverse <<- Inv
    getInv <- function() Inverse
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## 

cacheSolve <- function(x, ...) {
    ## Return the inverse matrix of a square matrix.
    ## No check is made to see if the input matrix is singular.
    ## If the inverse is available in the cache, it is not calculated again.
    Inverse <- x$getInv()
    if(!is.null(Inverse)){
        message("getting cached data")
        return(Inverse)
    }
    M <- x$get()
    Inverse <- solve(M) #solve(M) finds the inverse of M
    x$setInv(Inverse) #cache the inverse
    Inverse
}
