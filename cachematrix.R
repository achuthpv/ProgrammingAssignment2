## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special list that can handle the background process of getting the x matrix as well as caching its inverse. It also takes care of resetting the cached inverse once x is changed
#inv stores the cached inverse value
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#this is the alternate solve function which takes care of handling the cached value of inverse of x matrix which is part of special list created above. It shows cached value if x unchanged since previous inverse calculation. If x is found to be changed, it will recalculate the inverse of x and also stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
