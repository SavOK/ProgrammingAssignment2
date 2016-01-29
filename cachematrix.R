## Calculates the inverse of matrix
## The MeakeCacheMatrix generates list of "pointer-like" location of where 
## matrix and its inverse stored

## Generate special struct like matrix object
## "Pointer" to the value of the matrix 
## Get the value of the matrix
## "Pointer" to the value of inverse 
## Get the value of inverse 
## Input: square matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
        set <- function(y) {
            x <<- y
                inverse <<- NULL
        }
    get <- function() {
        x
    }
    set_inverse <- function(mat) {
        inverse <<- mat
    }
    get_inverse <- function() {
        inverse
    }
    return (list (set=set, 
                get=get, 
                set_inverse=set_inverse, 
                get_inverse=get_inverse))
}

## Calculates inverse of matrix 
## Input: makeCacheMatrix list of matrix A (list is simular to C struct)
## Output: inverse of matrix A
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
        if(!is.null(inverse)) {  # very simular to check memory if not null then output
            message("getting cached inverse data")
            return(inverse)
        }
    data <- x$get()
    inverse <- solve(a=data, ...)
    x$set_inverse(inverse)
    return(inverse)
}
