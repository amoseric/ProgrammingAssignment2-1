## Using <<- operator to assign a value to an object in an environment that is 
## different from the current environment,  the two functions below use a 
## special object that stores a numeric matrix and cache's its inverse.

## makeCacheMatrix returns a list containing functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the mean
## 4) get the value of the mean

makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


# This function returns the inverse of the matrix. If the inverse has already 
# been computed, it gets the inverse from the cache and skips the computation. 
# Otherwise, it computes and sets the value of inverse in the cache.

cacheSolve <- function(x, ...) {  
    m <- x$getInverse()
    if (!is.null(m)) {
        print("result from cache:")
        return(m)
    } 
    m <- solve(x$get())
    x$setInverse(m)
    m
}

