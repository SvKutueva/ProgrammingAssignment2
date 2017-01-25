## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        save <- function(y) {
                x <<- y
                i <<- NULL
        }
        take <- function() x
        saveinverse <- function(inverse) i <<- inverse
        takeinverse <- function() i
        list(save = save, take = take,
             saveinverse = saveinverse, takeinverse = takeinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        i <- x$takeinverse()
        if(!is.null(i)) {
                message("taking cached data")
                return(i)
        }
        data <- x$take()
        i <- solve(data, ...)
        x$saveinverse(i)
        i
}
