## makeCacheMatrix() turns a square invertible matrix into a special object
## which is then used by cacheSolve() to compute its inverse using a cache if 
## available. tesSuiteCache() is for easily testing the 2 previous functions



## This function produces a list of 4 functions to get or set a  
## matrix and to get or set its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m_inverse <- NULL
    get <- function() x
    set <- function(y){
        m_inverse <<- NULL
        x <<- y
    }
    get_inverse <- function() m_inverse
    set_inverse <- function(inverse){
        m_inverse <<- inverse
    }
    list(get = get, set = set,
         get_inverse = get_inverse,
         set_inverse = set_inverse)
}


## Computes the inverse of the "matrix" object from the 
## function above, returning a cached version if available

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x which  
    ## must be produced by makeCacheMatrix
    
    m_inverse <- x$get_inverse()
    if( !is.null(m_inverse)){
        message("Using cached inverse matrix")
        return(m_inverse)
    }
    message("computing inverse matrix")
    m_inverse <- solve(x$get(), ...)
    x$set_inverse(m_inverse)
    m_inverse
}


testSuiteCache <- function(m = matrix(1:4,2,2), ...){
    
    ## this should compute the inverse, then use the cached inverse
    
    special_m <- makeCacheMatrix(m)
    inverse_m <- cacheSolve(special_m, ...)
    inverse_m <- cacheSolve(special_m, ...) 
    
    ## resetting m should ensure recomputation of inverse matrix
    
    special_m$set(m)
    inverse_m <- cacheSolve(special_m) 
    
    ## checking the actual computation of the inverse
    message('initial matrix: ')
    print(m)
    message('inverse matrix: ')
    print(inverse_m) 
    message('matrix multiplication with inverse matrix: ')
    print(m %*% inverse_m)
}




