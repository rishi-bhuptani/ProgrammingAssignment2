## Functions will create a way to cache matrix inverses

## Creates methods to use for matrices and special vectors for use in second function

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set_matrix <- function(y)
                {
                x <<- y
                m <<- NULL
                }
        get_matrix <- function() x
        set_inverse <- function(inverse_matrix) m <<- inverse_matrix
        get_inverse <- function() m
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Checks to see if the inverse for a named matrix is cached and returns it; if not, inverses the entered
##matrix and caches it

cacheSolve <- function(x, ...) 
{
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get_matrix()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}
