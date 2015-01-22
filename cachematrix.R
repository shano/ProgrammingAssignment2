## This code provides a cacheable way to 
## invert matrices.

## This is a object that takes in a matrix
## and provides functions to return that matrix and also 
## caches and return the inverse of the matrix
makeCacheMatrix <- function(mat = matrix()) {
        inv = NULL
        # Set the internal variable mat with supplied matrix
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        # Return matrix
        get <- function() mat
        # Set the internal variable inv with the inverse
        setinverse <- function(inverse) inv <<- inverse
        # Return matrix inverse
        getinverse <- function() inv
        # Return the list of callable arguments
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function takes in a makeCacheMatrix object and returns it's inverse
cacheSolve <- function(x, ...) {
        # Tries to get a cached copy and return
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # If none gets the matrix
        data <- x$get()
        # Calculates it's inverse
        inverse <- solve(data, ...)
        # And sets the inverse on the makeCacheMatrix container
        x$setinverse(inverse)
        inverse
}
mat = matrix(c(4,3,3,2), 2)
print(mat)
cacheMat = makeCacheMatrix(mat)
inverseMat = cacheSolve(cacheMat)
inverseMat = cacheSolve(cacheMat)
print(inverseMat)