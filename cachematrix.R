# makeCacheMatrix
# Creates a special "matrix" object that can cache its inverse.
# The object does not calculate the inverse, just saves it inside.
# Saves the matrix to variable x and its inverse to variable m in scope.
# Returned object (actually it's a list) contains methods:
# set: sets matrix and resets cached inverse
# get: returns matrix
# setSolve: saves solve value
# getSolve: returns cached inverse value

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get, 
         setSolve = setSolve, 
         getSolve = getSolve)
}

# Function to get the inversed matrix from a special object created by makeCacheMatrix.
# Takes the object of that type as an argument 'x', checks if the inverse value is already
# cached, and if it is returns the cached value; if not, this function calculates the
# inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setSolve'
# and returns the result.
cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
