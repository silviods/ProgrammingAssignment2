## Put comments here that give an overall description of what your
## functions do

## SEE MY UNIT TEST IN THE END OF THIS CODE

## Write a short comment describing this function
## Objective: declare functions in a list, to:
##      1 - set the value of the vector
##      2 - get the value of the vector
##      3 - set the value of the matrix inverse using solve
##      4 - get the value of the matrix inverse
## This returns a list containing the functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    myfunctionlist <- list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    return(myfunctionlist)
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
    
    ## caches the matrix inverse, and returns it
    x$setinverse(matrixinverse)
    return(matrixinverse)
}

## Unit Test (example)
## > source("cachematrix.R")
## > m1 <- matrix(c(2, 4, 3, 1, 5, 7, 7, 4, 2, 6, 5, 9, 2, 4, 8, 9), nrow=4, ncol=4)
## > m2 <- makeCacheMatrix(m1)
## > cacheSolve(m2)
##          [,1]        [,2]       [,3]        [,4]
## [1,] -1.06842105  0.58421053  0.3894737 -0.36842105
## [2,]  0.70526316 -0.25263158 -0.1684211  0.10526316
## [3,]  0.05789474  0.12105263 -0.2526316  0.15789474
## [4,] -0.25263158 -0.07368421  0.2842105 -0.05263158
## 
## if you call cacheSolve a secont time, it will show you message
## "getting cached data" and then return the inverted matrix
