## Calculates the invers of a matrix. chachesolve reads the isolve
## from the cache, if the insolve of the object was calculated earlier
## and the matrix did not change.

## creates a special "vector", which is
#  a list containing a function to

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the invers
# 4.  get the value of the invers

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## calculates the solve of the special vector
## created with the makeChacheMatrix function.
## It first checks to see if the
## solve has already been calculated.
## If so, it `get`s the solve from the
## cache and skips the computation.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
