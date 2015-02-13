## makeCacheMatrix and cacheSolve functions permit to reduce the amount of memory and time  
## used for computing a matrix's inverse by storing previously computed result in cache 
## within function makeCacheMatrix and preventing the same computational operation of cacheSolve.

## makeCacheMatrix creates a vector with 4 functions inside, which permit to store a matrix value
## by x$set (and to get it by x$get) and to store the inverse of a matrix by x$setinverse (and to 
## get it by x$getinverse).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function first checks whether we have calculated the inverse of a matrix already by trying
## to use x$getinverse() - if it exists, the function writes a message and returns previously calculated
## result. If we haven't calculated and putted the inverse into cache by makeCacheMatrix, it tries to
## make the inverse by m <- solve(data, ...) and put the result in cache. So, the next time we will be
## able to use the result from cache.
## !!! IMPORTANT - we also need to check that the matrix has not changed - by identical(existingMatrix,x)
## I assume that there is always some matrix x in cache (without checking its existence)

cacheSolve <- function(x, ...) {
        
        existingMatrix <- x$get()
        if (identical(existingMatrix,x)) {
                m <- x$getinverse()
                if (!is.null(m)) {
                        message("getting cashed data")
                        return(m)
                }  
                ## if there is no previously calculated inverse
                else {
                        data <- x$get()
                        m <- solve(data, ...)
                        x$setinverse(m)
                        m
                }
        }
        ## matrices are not identical
        else {
                x$set(x) 
                ## putting new matrix inside. We could make code easier, just by m <- solve(x, ...)
                ## but I'd like to change the matrix in cache
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m  
        }        
              
}
