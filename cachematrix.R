## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(sv) s <<- sv
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    sv <- x$getsolve()
    if(!is.null(sv)) {
        message("getting cached data")
        return(sv)
    }
    data <- x$get()
    sv <- solve(data, ...)
    x$setsolve(sv)
    sv
}

## test code
myData <- makeCacheMatrix()
myData$set(matrix(rnorm(200),10,10))

cacheSolve(myData)
cacheSolve(myData)