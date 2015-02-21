###   cacheMatrix.R--Two functions
###   makeCacheMatrix:  generate a special matrix that can cache its inverse
###   cacheSolve:  compute the inverse of a matrix or deliver the previously computed inverse

##   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    ##  Set the matrix and reset the environment
    set <- function(y)
        {
            x <<- y
            m <<- NULL
        }
    ##  Get the matrix
    get        <- function() x

    ##  Set the enivironment with the inverse of the matix
    setinverse <- function(inverse) m <<- inverse

    ##  Get the inverse matrix
    getinverse <- function() m
    list(set        = set,
         get        = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()

    ## see if the inverse has been cached
    if (!is.null(m)) 
        {
            message("getting cached data")
            return(m)
        }

    data <- x$get()                     # get the data
    m    <- solve(data, ...)            # compute the inverse
    x$setinverse(m)                     # remember (cache) the inverse
    m                                   # return the inverse
}
