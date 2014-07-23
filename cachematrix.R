## Calculate the inverse of a matrix
## How to use (with example code):
## 1. Create a matrix: cx<- matrix(c(4,2,7,6),nrow=2,ncol=2)
## 2. Create a special matrix object to store it in: bc <- makeCacheMatrix()
## 3. Add the matrix to the special matrix object: bc$set (cx)
## 4. Calculate the inverse of that matrix: cacheSolve(bc)
## Each time you run step 4 (after the first time) it will return the cached result
## If you pass a new matrix (step 3) it will recalculate in step 4

## makeCacheMatrix creates a matrix object that can cache the inverse of that object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## cacheSolve takes a matrix object from makeCacheMatrix and returns the inverse
## if the matrix inverse had previously been calculated it returns the cached version
## if it had not, then it calculates it with the solve function, stores the result
## in the cache, and returns it 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}