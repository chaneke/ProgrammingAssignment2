## cachematrix scripts contains a couple of funtions used to calculate the invese of an input matrix
## this functions improve its performance based in store a cached value to avoid to recalculate the
## inverse of the matrix in this has been already stored.

## makeCacheMatriz, stores the result od solve function applied over at the input parameter, 
## input parameter is a Matrix object
makeCacheMatrix <- function(x = matrix()) {
    matriz <- NULL
    ## Setter functions, is in charge to assign the value of the matriz to x object in a different Env
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Getter function, is used to return (get) the value of the object x stored in different Env
    get <- function() x
    ## Used to calcule the inverse of the matrix
    setsolve <- function(solve) matriz <<- solve
    getsolve <- function() matriz
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function used to store the value of matrix inverted value as a cached value
## x is a function that contains the setter, getters and the solution of inverted matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
