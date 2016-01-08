## Put comments here that give an overall description of what your
## functions do

setwd("/home/malte/Git/ProgrammingAssignment2")

# This function creates a list of closures which are used later to store values of
# the inverse as well as set and retrieve it again at later times
# retrieving is done by calling the function from the list created using this factory

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) i <<- inverse 
    getinverse <- function() i
    list <- list(set = set, get = get, setinverse = setinverse, 
                 getinverse = getinverse)
}

# This function retrieves the cached inverse from the closure in makeCacheMatrix
# If that i is defined as NULL, as done for a freshly created matrix, it uses
# the solve function to calculate the inverse of the matrix and then stores it
# in the closure of makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# Tests
matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(matrix)
cacheSolve(matrix)