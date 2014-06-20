## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        
        inverse_matrix_hold <- NULL
        #assigns the value of y to x and sets inverse_matrix null
        set <- function(y) {
                x <<- y
                inverse_matrix_hold <<- NULL
        }
        
        #assigns the value of x to get
        get <- function() x
        
        setinv <- function(inverse_matrix) inverse_matrix_hold <<- inverse_matrix
        getinv <- function() inverse_matrix_hold
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}

