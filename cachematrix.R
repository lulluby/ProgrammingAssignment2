## Here are two functions, which could help increase productivity
## when working with big matrices

## makeCacheMatrix gets a matrix as input, returns complex object, that
## incapsulates matrix itself, inverse matrix for the given matrix 
## and methods, which allow to set, get matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve gets an object, created by using makeCacheMatrix function,
## tries to get inverse matrix from the object given
## if inverse matrix hasn't been calculated and saved to the object given,
## calculates it and save to the object for later usage
## return inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
