## R Programming - Assignment 2
## create matrix object capable of cacheing its inverse
## and write function to compute inverse of matrix

## Create matrix object capable of cacheing its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Create function that computes inverse of matrix returned by
## makeCacheMatrix

cacheSolve <- function(x, ...) {
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


## some extra code to test whether the functions work

# 1. create matrix named bob
frank <- rnorm(100)
bob <- matrix(frank, 10, 10)

# 2. create second matrix named kyle that is the inverse of bob
kyle <- cacheSolve(makeCacheMatrix(bob))

# 3. take inverse of kyle (a.k.a inverse of bob) - inverse of
# kyle should equal bob, and it does
cacheSolve(makeCacheMatrix(kyle))