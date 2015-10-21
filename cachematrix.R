## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set_matrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        get_matrix <- function() x
        
        set_inverse <- function(inversem) m <<- inversem
        
        get_inverse <- function() m
        
        list(set_matrix = set_matrix, 
             get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached inverse of matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        m
}
