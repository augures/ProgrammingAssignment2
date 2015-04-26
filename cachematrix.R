## A pair of functions makeCacheMatrix and cacheSolve implement and ability 
## to cache matrix inverse for a given matrix.
## They are supposed to work together in a following way:
## makeCacheMatrix expects matrix, and returns specific object 
## (list of functions, see makeCaheMatrix comments for details),
## cacheSolve expects this object as input, and returns an inverse of  matrix
## represented by this object.

makeCacheMatrix <- function(x = matrix()) {
    # makeCacheMatrix takes a matrix as an input,
    # it stores this matrix in an environment asssociated with a function,
    # and returns a list of getter/setter functions for a matrix and it's inverse:
    # list(
    #   setMatrix: function,    
    #   getMatrix: function,
    #   setMatrixInverse: function,
    #   getMatrixInverse: function
    # )
    # setMatrix stores provided matrix in functions's environment,
    #   and unsets a value that getMatrixInverse returns.
    # getMatrix retrieves matrix provided initialy or stored by setMatrix
    # setMatrixInverse stores matrix (that is supposed to be an inverse
    #   of the matrix stored initially of by setMatrix)
    # getMatrixInverse retrieves a matrix stored by  setMatrixInverse
    # PLEASE NOTE: it's possible to call setMatrixInverse with arbitrary matrix,
    #   that is not an inverse of matrix set by setMatrix.
    #   It's an obligation of calling code (cacheSolve in this case)
    #   to use getters/setters appropriately.
    
    # two variables are defined in function's environment:
    # x - it already defined in function arguments
    # matrixInversed - we need to define it explicitly
    # we will use them in functions defined below via <<- operator
    matrixInversed <- NULL
    
    # stores matrix value, and unset inversed matrix value,
    # as it may not correspond to matrix any more
    # please note <<- operator
    setMatrix <- function(y) {
        x <<- y
        matrixInversed <<- NULL
    }
    
    # returns matrix value stored in parent environment
    # (it's retrieved from parent environment, 
    # as there is no variable with name 'x' defined in getMatrix  environment)
    getMatrix <- function() x
    
    # stores matrix inverse in parent environment (please note <<- operator)
    setMatrixInverse <- function(mi) matrixInversed <<- mi
    
    # returns matrix inverse value stored in parent environment
    # (it's retrieved from parent environment, 
    # as there is no variable with name 'xmatrixInversed'
    #   defined in getMatrixInverse environment)
    getMatrixInverse <- function() matrixInversed
    
    # return list of functions defined above
    list(getMatrix=getMatrix,
         setMatrix=setMatrix,
         getMatrixInverse=getMatrixInverse,
         setMatrixInverse=setMatrixInverse)
}

cacheSolve <- function(x, ...) {
    # cacheSolve function expects a list of functions returned by makeCacheMatrix
    # cacheSolve uses those functions to retrieve cached inverse of matrix
    #   stored in this list of functions, if this cached value is available.
    # If not, it retrieves a matrix, calculates  inverse, and caches it
    #   using functions from input list of functions.
    # cacheSolve ouputs a message on whether an inverse was calculated,
    #   or retrieved from cache.
    mi <- x$getMatrixInverse()
    if (is.null(mi)) {
        message('matrix inverse is absent in cache, calculating...')
        xMatrix <- x$getMatrix()
        mi <- solve(xMatrix)
        x$setMatrixInverse(mi)
        message('matrix inverse is calculated and stored in cache')
    }
    else {
        message('matrix inverse is retrieved from cache')
    }
    mi
}