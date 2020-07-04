## Put comments here that give an overall description of what your
## functions do
#
# 'makeCacheMatrix' sets up a special matrix with functions to cache the inverse
# 'cacheSolve' is used after 'makeCacheMatrix' object initiated and set, it will then setInverse


## Write a short comment describing this function
# init an instance of the special matrix by calling base function
#
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # set null on initialization
    set <- function(y){
        x <<- y  # push the input variable to special matrix
        m <<- NULL # push NULL to the parent's variable (reset inverse when change matrix)
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv   # relies on external function to store inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)  # returning the functions to the special matrix on init
}


## Write a short comment describing this function
# call 'cacheSolve' with parameter x as an instance of 'makeCacheMatrix'
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse() # try to pull cached inverse result
    if(!is.null(m)) {
        message("getting cached data")
        return(m)  # this exits the function 'cacheSolve' and stops executing rest of the function
    }
    
    # if inverse is null...
    data <- x$get()  #get matrix values
    m <- solve(data,...)  # solve for inverse matrix
    x$setinverse(m)  # store inverse matrix back in the special matrix functions
    m  # return the inverse matrix to original caller
}
