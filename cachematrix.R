## Put comments here that give an overall description of what your
## functions do

## Creating a cache of matrix for the other function use 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) m <<- Inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## To check if the cache has been set in the function makeCacheMatrix
## If so, get it; if not, make one 2 on 2 matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()           #query the x Inverse's cache         
    if(!is.null(m)) {           #if there is a cache
        message("getting cached data") 
        return(m)                #just return the cache, no computation needed
    }

    Iv <- x$get()             #if there's no cache
    det <- (Iv[1,1]*Iv[2,2] - Iv[1,2]*Iv[2,1])
    m <- 1/(det)*matrix(c(Iv[2,2], Iv[2,1], Iv[1,2], Iv[1,1]), 2)        #we actually compute them here
    x$setInverse(m)                #save the result back to x's cache
    m                           #return the result
}
