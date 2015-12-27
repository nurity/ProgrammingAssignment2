## The functions work together to create a square invertible matrix, available in
## the cache environment

## First function: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inverse_matrix <- NULL 
    set_matrix <- function(y) { 
        x <<- y
        inverse_matrix <<- NULL
    }
    get_matrix <- function() { 
        x
    }
    set_inverse <- function(inverse) { 
        inverse_matrix <<- inverse
    }
    get_inverse <- function() { 
        inverse_matrix
    }
    list(set_matrix = set_matrix,
        get_matrix = get_matrix,
        set_inverse = set_inverse,
        get_inverse = get_inverse)
}

## Second function: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above
cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        return(inverse) 
    }
    data <- x$get_matrix()
    inverse <- solve(data, ...) 
    x$set_inverse(inverse)
    inverse
}
