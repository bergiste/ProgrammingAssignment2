## Gets the inverse of a matrix using the solve function. Caches the results for speed and efficiency

## Creates a cached version of a matrix

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


## Get the inverse of a matrix (solve) but caches the results to avoid repeating the costly operation

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

## Test the code above
myData <- makeCacheMatrix()
myData$set(matrix(rnorm(200),10,10))

cacheSolve(myData)
cacheSolve(myData)