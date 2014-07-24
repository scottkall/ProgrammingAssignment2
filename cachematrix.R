## The purpose of this script is to cache the inverse of a matrix.

## makeCacheMatrix creates a set of functions to
# 1. set the values of the initial matrix, 'x'
# 2. get the matrix
# 3. set the values of the inverted matrix, 'x.i'
# 4. get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        x.i <- NULL                            
        set <- function(y) {                   
                x <<- y                       
                x.i <<- NULL                
        }
        get <- function() x                  
        setinv <- function(solve) x.i <<- solve  
        getinv <- function() x.i              
        list(set = set, get = get,           
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of the special matrix created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x.i <- x$getinv()                       
        if(!is.null(x.i)) {                      
                message("getting cached data")
                return(x.i)
        }
        data <- x$get()                        
        x.i <- solve(data, ...)                 
        x$setinv(x.i)                           
        x.i
}
