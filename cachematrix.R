# This function creates a special "matrix" object that can cache its
# inverse.

# This function, makeCacheMatrix creates a special "matrix", which is
# really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}


# Sample output: 
# > M <- matrix(1:4, nrow = 2, ncol = 2)
# > M
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# > CM <- makeCacheMatrix(M)
# > CM$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# First run:
# > cacheSolve(CM)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Second run:
# > cacheSolve(CM)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > M %*% cacheSolve(CM)
# getting cached data
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
