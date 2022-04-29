## Using two functions, I will be able to write a function of a matrix that
## can cache the inverse of another matrix

## makeCacheMatrix function creates a matrix that can cache its inversion for
## the output. I then initialize inv as Null, and set/get the matrix x. Then, I
## create a function to set/get the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, 
             get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## The function, cacheSolve gets cached data (function will return inverse
## of makeCacheMatrix's function value of x). First, I create an if() 
## statement to either output a message if inv is null, or return the
## inversed matrix of makeCacheMatrix if inv != Null.
## To test if the inversion worked, I used the function cacheSolve to see
## if the inversed function can be retrieved.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached result")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## -- Testing --- ##
## m <- matrix(rnorm(4),2,2)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)
