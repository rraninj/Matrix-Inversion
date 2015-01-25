makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(y) {
        m <<- y
        inverse <<- NULL
    }
    get <- function() {
        m
    }
    setinverse <- function(inv) {
        inverse <<- inv
    }
    getinverse <- function() {
        inverse
    }
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}