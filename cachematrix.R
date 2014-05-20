## This pair of functions will cache the inverse of a matrix

## The first funcion, makeCacheMatrix, creates a special matrix object. It is
## really a list containing a function to 
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function()x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function () inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function, cacheSolve, computes the inverse of the matrix returned by 
## makeCacheMatrix. If the matix hasn't changed and the inverse has already been 
## calcuated, then cacheSolve should return the inverse from the cache,
## not compute it again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

