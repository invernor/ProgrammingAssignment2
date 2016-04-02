
## This function creates a special "matrix" object that can cache its inverse

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverse) im <<- inverse
        getInverseMatrix <- function() im
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        
        im <- x$getInverseMatrix()
        if (!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        mtrx <- x$get()
        im <- solve(mtrx, ... )
        x$setInverse(im)
        im
}
