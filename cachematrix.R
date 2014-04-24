## Following pair of functions create a special matrix which can cache its own inverse. If the inverse is computed
## earlier, then it uses the cache from special matrix instead of calculating it again. Pretty cool, eh?

## makeCacheMatrix creates a special type of matrix and returns a list containing functions which set and get 
## the matrix value from the special matrix and functions which set the inverse of the matrix and 
## return the inverse of the matrix.
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


## cacheSolve function either computes the inverse matrix if not computed earlier else it 
## retrieves from the cache, without computing it again.

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
