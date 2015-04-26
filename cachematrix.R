#########################################################
## makeCacheMatrix ()
## this function creates a matrix object that can cache
## it's invers.
#########################################################
makeCacheMatrix <- function(x = matrix()) 
{
    ## initialize our value
    cachedValue <- NULL
    
    ## set a new matrix value
    ## we reset the cached value, since this is a new matrix
    set <- function(y) {
        x <<- y
        cachedValue <<- NULL
    }
    
    ## return the matrix
    get <- function() {
        x
    }
    
    ## set the cached value... the matrix inverse
    setInverse <- function(value) {
        cachedValue <<- value
    }
    
    ## return the cached inverse
    getInverse <- function() {
        cachedValue
    }
    
    ## list of functions
    list( set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse )

}



#########################################################
## cacheSolve ()
## returns a matrix that is the inverse of 'x'
## checks to see if there is a cached version that can
## be used and prints a message if cached version
## is found/used
#########################################################
cacheSolve <- function(x, ...) 
{
    ## get the value from the matrix
    cachedValue <- x$getInverse()
    
    ## see if we have a cached value to use...
    if (!is.null(cachedValue)) {
        message ("using cached value")
        return (cachedValue)
    }
    
    ## at this point we have no stored value, so we solve() 
    data <- x$get()
    cachedValue <- solve(data, ...)
    
    ## now cache the result
    x$setInverse(cachedValue)
    
    ## ... and return the result
    cachedValue
    
}


###############################################################
##  timedTest()
##  a simmple test based on example posted by "Karl Schultz"
###############################################################
timedTest <- function(size)
{
    n <- size
    
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makeCacheMatrix(mat)
    time1 <- system.time(matSolved1 <- cacheSolve(matCached))
    time2 <- system.time(matSolved2 <- cacheSolve(matCached))
    if (time1["user.self"] < time2["user.self"])
        message(sprintf("WARNING! Solve time is less than cache time: solveTime=%s cacheTime=%s", 
                        time1["user.self"], 
                        time2["user.self"]))
    else 
        message (sprintf("Caching is faster than solving: solveTime=%s cacheTime=%s", 
                         time1["user.self"], 
                         time2["user.self"]))
}
