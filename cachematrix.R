## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inverse.mat <- NULL
        set <- function(y) {
        x <<- y
        inverse.mat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse.mat <<- inverse
        getinverse <- function() inverse.mat
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse.mat <- x$getinverse()
        if (!is.null(inverse.mat)) {
                message("getting cached data")
                return (inverse.mat)
        } else {
                data <- x$get()
                inverse.mat <- solve(data, ...) # only for square invertible matrix
                x$setinverse(inverse.mat)
                return (inverse.mat)
        }
}
