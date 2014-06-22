# Implementing a special version of "solve()" called "cacheSolve()"
# which will return the inverse, but without calculating it and rather
# fetching it from a cache if it was calculated before during program
# execution.

# Function makeCacheMatrix
# Creates a special matrix which is able to cache its inverse
# Implemented as a list of the following functions:
# set: define the matrix
# get: get the matrix defined by set
# setinv: calculate the inverse and cache it using solve()
# getinv: gets the cached inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix(c(NA),dim(x)[1],dim(x)[2])
        set <- function(y) {
                x <<- y
                inv <<- matrix(c(NA),dim(y)[1],dim(y)[2])
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# Function cacheSolve
# Calculates the inverse of a matrix, either by
# 1) getting the inverse already calculated before from the cache, or
# 2) calculating it using solve() and caching it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.na(det(inv))) {
                message("Getting cached inverse...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
