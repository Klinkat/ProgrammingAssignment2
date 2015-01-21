
##Below are two functions, makeCacheMatrix() and cacheSolve(), which are meant to work together to: 
##    1. inverse a matrix and save the result in a cache
##    2. read the inverse matrix from the cache instead of recalculating it, if the input is the same matrix


##  makeCacheMatrix() takes a matrix as argument and returns a list of 4 elements that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {             
    inv <- NULL                                 ## "inv" is set to NULL (only in the global environment)
    set <- function(y){                         ## in case the matrix changes
        x <<- y                                 ## reads in a matrix (indepentent of the parent function), not part of the standard procedure
        inv <<- NULL                            ## "inv" in the parent environments (cache) is erased, set to NULL
    }
    get <- function() x                         ## reads matrix = x
    setinv <- function(invert) inv <<- invert   ## stores the function input into the cache "inv"
    getinv <- function() inv                    ## reads "inv" 
    list (set = set, get = get,                 ## a list is the outout of this function, 
          setinv = setinv,                      ##      naming each element of the list the same as the function it contains
          getinvn = getinv)
}


## cacheSolve() takes the list of makeCacheMatrix() as argument, calculates the inverse or retrieves it from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                   ## reads the cache (formerly produced inverse or NULL) and saves it in "inv" 
                                        ## (goes to the "getinv" element of makeCacheMatrix, which is a function to read "inv"

    if(!is.null(inv)){                  ## in case the cache is not NULL:
        message("getting cached data")      ## message is printed
        return(inv)                         ## cacheSolve() returns inverse matrix without re-calculating
    } 
                                        
                                        ## in case cache is NULL
    data <- x$get()                     ## reads matrix = x that was supplied for makeCacheMatrix() via makeCacheMatrix$get
    inv <- solve (data)                 ## inverses matrix and stores it in "inv" (only global environment)
    x$setinv(inv)                       ## writes "inv" to the setinv-element of makeCacheMatrix(), where it is stored into the cache
    inv                                 ## the inverse matrix is the output of this function
}
