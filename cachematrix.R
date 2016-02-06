## Some processes take a long time to be done. Therefore it is better to 
## preprocess some computing and cache it. Now you do not have to
## compute matrix inverse repeteadly. These functions were made
## in order to cache inverse of a matrix. 

## This function creates a list containing values of get the matrix, 
## set the matrix, set inverse of the matrix and get inverse of the matrix. 
makeCacheMatrix <- function(x = matrix()) {
        cached <- NULL
        set <- function(y) {
                x <<- y
                cached <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) cached <<- solve
        getinverse <- function() cached
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This functions returns inverse of our matrix. First of all 
## function checks whether inverse has been computed and if 
## it has been computed, function returns the value of inverse. 
## If not, function computes the inverse using setinverse function.
cacheSolve <- function(x, ...) {
        cached <- x$getinverse()
        if(!is.null(cached)) {
                message("getting cached data")
                return(cached)
        }
        data <- x$get()
        cached <- solve(data, ...)
        x$setinverse(cached)
        cached
}
