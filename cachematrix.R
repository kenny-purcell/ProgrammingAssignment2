## The function below work together to efficiently calculate the inverse
## of a matrix saving the calculated value for reuse. Use them as follows:
##
## First call makeCacheMatrix with a square, invertible matrix as the
## argument and save the return value.
##
##  cached_matrix <-  makeCacheMatrix(my_matrix)
##
## Then call cacheSolve to get the inverse of your matrix.
##
##  cacheSolve(cached_matrix)
##
## When the cached inverse is returned a message is printed on the console
## to let you know the the stored value is being used.

## ------------------------ makeCacheMatrix ----------------------------
## Return a vector of the following functions:
## set - Stores the value of it argument, which is presumed to be a matrix.
##       This function also resets the internally stored matrix inverse to
##       NULL.
## get - Returns the value stored by "set".
## setinv - Stores the value of its argument, which is presumed to be the
##          inverse of the matrix stored by "set".
## getinv - Returns the value stored by "setinv".
##
## These four function share a common environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## ---------------------------- cacheSolve -----------------------------
## Return the interverse of the matrix stored in the argument x.  If x has
## a stored value of the inverse, that value is returned.  If the stored
## inverse is NULL, the inverse is calculated and saved in x before the
## value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
