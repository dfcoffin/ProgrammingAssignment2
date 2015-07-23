#################################################################
##                                                             ##
##     R Programming - Programming Assignment 2                ##
##                                                             ##
##     Matrix inversion is usually a costly computation.       ##
##     Caching the inverse of a matrix rather than always      ##
##     computing it repeatedly (if it hasn't changed)          ##
##     may provide some performance benefits.                  ##
##                                                             ##
##     The following two functions provide the following       ##
##     abilities:                                              ##
##                                                             ##
##     makeCacheMatrix - Creates a special "matrix" object     ##
##                       that can cache its inverse.           ##
##                                                             ##
##     cacheSolve      - Computes the inverse of the           ##
##                       special "matrix" returned by the      ##
##                       makeCacheMatrix function. If the      ##
##                       inverse has already been              ##
##                       calculated (and the matrix has        ##
##                       not changed) cacheSolve retrieves     ##
##                       the inverse from cache.               ##
##                                                             ##
#################################################################
#################################################################
##                                                             ##
##     makeCacheMatrix:                                        ##
##                                                             ##
##     The following function creates an inverse matrix        ##
##                                                             ##
##     variables                                               ##
##     =========                                               ##
##         m     - Contains cached inverse matrix              ##
##                                                             ##
#################################################################

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL                  ##   Initialize cached inverse
                               ##   matrix

    
    #############################################################
    ##                                                         ##
    ##   Set function - Saves original matrix                  ##
    ##                                                         ##
    ##   variables:                                            ##
    ##   ==========                                            ##
    ##       y      - Contains matrix to be inversed           ##
    ##       m      - Contains cached inversed matrix          ##
    ##                                                         ##
    #############################################################
    
    set <- function(y) { 
        
        x <<- y                ##   Save original matrix
        m <<- NULL             ##   Clear cached inverse matrix
    }
    
    #############################################################
    ##                                                         ##
    ##   Get function - Returns original matrix                ##
    ##                                                         ##
    ##   variables:                                            ##
    ##   ==========                                            ##
    ##       x      - Contains matrix to be inversed           ##
    ##                                                         ##
    #############################################################
    
    get <- function() x 
    
    #############################################################
    ##                                                         ##
    ##   setinverse function - Saves inversed matrix in        ##
    ##                         cached variable                 ##
    ##                                                         ##
    ##   variables:                                            ##
    ##   ==========                                            ##
    ##    inverse   - Contains inverse matrix to be saved      ##
    ##       x      - Cached inverse matrix                    ##
    ##                                                         ##
    #############################################################
    
    setinverse <- function(inverse) m <<- inverse
    
    #############################################################
    ##                                                         ##
    ##   getinverse function - Returns inverse matrix          ##
    ##                                                         ##
    ##   variables:                                            ##
    ##   ==========                                            ##
    ##       m      - Cached inverse matrix                    ##
    ##                                                         ##
    #############################################################
    
    getinverse <- function() m 
    
    list(set = set,            ##   Create list of functions
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


#################################################################
##                                                             ##
##     cacheSolve:                                             ##
##                                                             ##
##     The following function returns a matrix that is         ##
##     the inverse of 'x'                                      ##
##                                                             ##
##                                                             ##
#################################################################


cacheSolve <- function(x, ...) {

    m <- x$getinverse()  ## Retrieve cached inverse matrix
    
    if(!is.null(m)) {    ## Has inverse matrix been calculated?
        message("getting cached data")
        return(m)        ##   Yes, return cached value
    }
    
    
    data <- x$get()      ##   No, calculate inverse matrix
    m <- solve(data)
    x$setinverse(m)      ## Save calculated inverse matrix in cache
    m
    
}
