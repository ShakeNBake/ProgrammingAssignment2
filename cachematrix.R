## Use below functions to 
## 1. create a special list object that can be used to set a matrix object, 
##    retrieve it, set its inverse, and retrieve the inverse.
## 2. Calculate and cache inverse of a matrix in one 
## functions do

## makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to:
## * set the value of the matrix
## * get the value of the matrix
## * set the value of the inverse matrix
## * get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        
        ## Set inverse matrix to NULL
        m <- NULL
        
        ## Sets the matrix, clears out inverse matrix
        set_matrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Retrieves the matrix set using set_matrix function
        get_matrix <- function() x
        
        ## Caches the inverse of a matrix
        set_inverse <- function(inversem) m <<- inversem
        
        ## Retrieves the inverse of a matrix
        get_inverse <- function() m
        
        ## Return the list of function to create the list object
        list(set_matrix = set_matrix, 
             get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve will retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        ## If inverse has already been stored, use the cached version
        if(!is.null(m)) {
                message("getting cached inverse of matrix")
                return(m)
        }
        
        ## If inverse hasn't been cached, calculate it and cache the result
        data <- x$get_matrix()
        m <- solve(data, ...)
        x$set_inverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
