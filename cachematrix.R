## Both the below functions are used to cache the matrix inverse function

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL ## Variable to cache the inverse value
    xcache <- NULL ## Variable to cache the matrix for which the inverse was calculated
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    isidentical <- function() identical(getCache(), get())
    setCache <- function(c) {
        xcache <<- c
    }
    getCache <- function() xcache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse,
         getCache = getCache, setCache = setCache,
         isidentical = isidentical)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse() ## Get the inverse of the matrix from the cache
    data <- x$get() ## Get the original matrix
    
    if(!is.null(i)){ ## Check if the inverse already exists
        if(x$isidentical()) { ## Check if the matrix is similar
            message("getting cached data")
            return(i)
        }
    }
    
    i <- solve(data, ...)
    x$setinverse(i)
    x$setCache(data)
    i
 }
