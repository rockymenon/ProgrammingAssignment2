## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix(): creates a special matrix that can cache its inverse.
## cacheSolve(): computes the inverse of the matrix returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, it will retrieve the inverse from the cache directly.

## makeCacheMatrix():
## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##              this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
 inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}

## cacheSolve():
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
