## makeCacheMatrix creates a special matrix which can cache its inverse
## computes the inverse of the special matrix

## makeCacheMatrix:
## 1) set matrix value
## 2) get matrix value
## 3) set matrix inverse
## 4) get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
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


## cacheSolve:
## 1) Calculates inverse of the special matrix
## 2) But if already calculated, just retrieves it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("Retrieving cached data!")
        return(m)
    }
    data <- x$get
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
