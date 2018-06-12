## These are a pair of functions that compute the value of inverse of
## a matrix and cache its result so that it may be used afterwards 
## without the hassel of computing

## This function creates a special 'matrix' object that can cache its 
## inverse and contains the functions for setting and getting the value 
## of vector and  setting and getting the value of inverse of the vector

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special matrix stored in the 
## output of the makeCacheMatrix(x = matrix()) above and stores it in the 
## cache. If the inverse has already been computed and stored, it would 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message('Getting cached data')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
    
        ## Returns a matrix that is the inverse of 'x'
}
