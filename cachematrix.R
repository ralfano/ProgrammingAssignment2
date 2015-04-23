## The function creates a special "vector", which is really a 
## list containing a function to:
##      - set the value of the matrix
##      - get the value of the matrix
##      - set the inverse of the matrix
##      - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function (y) {
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


## The function calculates the inverse of the special "matrix" 
## created with the function "makeCacheMatrix". 
## It first checks to see if the inverse has already been calculated. 
##      - If so, it gets the inverse from the cache
##      - Otherwise, it calculates the inverse and sets the inverse in the cache

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
