## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Objective: declare functions in a list, to:
## 1 - set the value of the vector
## 2 - get the value of the vector
## 3 - set the value of the matrix inverse using solve
## 4 - get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## Computes, caches and return matrix inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## from the x closure (makeCacheMatrix), gets the inverse
    matrixinverse <- x$getinverse()
    
    ## if inverse was cached, gives message and returns content
    if (!is.null(matrixinverse)) {
        message("getting cached data")
        return(matrixinverse)
    }
    
    ## if inverse was not cached:
    ## get original matrix from closure
    data <- x$get()
    
    ## calculates the inverse using solve
    matrixinverse <- solve(data, ...)
    
    ## caches the matrix inverse, and returns the 
    x$setinverse(matrixinverse)
    matrixinverse
}
