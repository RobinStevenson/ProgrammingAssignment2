## These functions define a matrix that can cache its inverse and a method
## for calculating/retrieving the inverse. 

## This function creates the matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Returns an object that contains a matrix and can cache its inverse
    inv <- NULL   #on initialisation the cached inverse is set to null
    set <- function(y) {
        x <<- y #The 'set' function sets the matrix to the argument y
        inv <<- NULL  #and removes any cached inverse
    }
    get <- function() x #'get' returns the stored matrix
    setInverse <- function(inverse) inv <<- inverse #This sets the cached inverse to the value passed in 
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the inverse of x and stores it if it is not
## already cached, or retrieves the cached inverse if it does exist.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        #If there is already a cached inverse, the cached value is returned
        message("getting cached data")
        return(inv)
    }
    data <- x$get() #If there is no cached inverse then the inverse is calculated
    inv <- solve(data, ...)
    x$setInverse(inv) # the calculated inverse is cached
    inv # the calculated inverse is returned
}
