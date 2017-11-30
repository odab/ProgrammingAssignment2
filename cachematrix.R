## File cachematrix.R
## ==================
## 
## Functions for operating on an matrix encapsulated in a closure.
## makeCacheMatrix() creates the closure, providing getters and setters for
## both the matrix and its inverse. Utility function cacheSolve() operates
## on the closure, returning either a calculated or cached matrix inverse.

## Stores the matrix provided, and provides getters and setters for both the
## matrix and its inverse. These getters and setters are implemented as
## function closures. The matrix inverse is cached, but must be explicitly
## saved via a call to setInv(). The matrix inverse is initialised to NULL,
## and is again set to null when the stored matrix is set via setMat().
## 
## Args:
##  x   A matrix
## 
## Returns:
##  A list of functions to operate on the matrix and it's stored inverse:
##      setMat(m)       Sets the stored matrix to m, and the stored inverse
##                      matrix to NULL. Does not return anything.
##      getMat()        Returns the stored matrix.
##      setInv(m)       Sets the stored inverse matrix to . Does not return
##                      anything.
##      getInv()        Returns the stored inverse matrix.
## 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMat <- function (y) {
                x <<- y
                inv <<- NULL
        }
        getMat <- function () x
        setInv <- function (inverse) inv <<- inverse
        getInv <- function () inv
        list(setMat = setMat, getMat = getMat, setInv = setInv,
             getInv = getInv)
}

## Takes a closure x created by makeCacheMatrix() and returns the inverse of
## the matrix stored in the closure. If the closure x has cached the inverse
## matrix, then cacheSolve(x) returns this cached matrix - otherwise
## cacheSolve(x) calculates the inverse, and caches it in the closure x for
## future computational convenience.
## 
## Args:
##  x   A closure x created by makeCacheMatrix()
##  ... Args to pass through to solve() along with the matrix from the closure
## 
## Returns:
##  The inverse of the matrix stored in closure x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (is.null(inv)) {
                mat <- x$getMat()
                inv <- solve(mat, ...)
                x$setInv(inv)
        }
        inv
}

