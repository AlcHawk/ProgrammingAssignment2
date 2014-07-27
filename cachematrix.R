## Put comments here that give an overall description of what your
## functions do
## Creating a cache of matrix for the other function use 
## Write a short comment describing this function

# set   : set the value of the matrix
# get   : get the value of the matrix
# setinverse: set the value of the inverse matrix
# getinverse: get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse   <- NULL               # inverse
        set <- function(y) {            # construct a func that sets x to its input param
                x <<- y                 # set to new matrix
                inverse <<- NULL        # matrix had changed - inverse sets to 0
        }
        get     <- function() x                            # construct a func that returns x
        setinverse <- function(inv) inverse <<- inv        # construct a func that sets "inverse" to its input param
        getinverse <- function()    inverse                # construct a func that returns i
        
        list(set = set, get = get, setinv = setinverse, getinv = getinverse) # return list of functions

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()                           # try getting inverse matrix from the cache
        if (!is.null(inv)) {                        # if inverse matrix was calculated before
                message("This is from the Cache")
                return(inv)                         # return the cached inverse
        }
        
        data <- x$get()                            # get the cached matrix
        inv    <- solve(data, ...)                 # caclulate its inverse
        
        x$setinv(inv)                              # cache the inverse matrix
        inv                                        # return the inverse matrix
        ## Return a matrix that is the inverse of 'x'
}
