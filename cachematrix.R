## These two functions allow caching the inverse of a matrix.
#         
#         To use these functions, one must first set up a special structure that will hold
#         the cache and then call the second function on that structure.
# 
#         The first function, makeCacheMatrix, sets up the structure.  Its input parameter is 
#         a matrix, and its output is four-part list that is actually the four functions to
#         (a) get the matrix, (b) set the matrix, (c) set the inverse, and (d) get the inverse.
# 
#         The second function, cacheSolve, calls makeCacheMatrix to see if the cache is already 
#         present.  If the cache is received, cacheSolve returns that value; if the cache is not
#         available, cacheSolve computes the inverse of the matrix and passes it makeCacheMatrix
#         to store.  The input parameter for cacheSolve is a matrix and the output is the inverse
#         of the matrix.  If the inverse was found in cache, cacheSolve displays a message to
#         that effect before returning the inverse.
# 
#         Example:
#         1. Create the cache:  a <- makeCacheMatrix(matrix(1:4,2))
#         2. Compute inverse:   b <- cacheSolve(a)
#         You may also call subfunctions directly:
#         3. Get matrix:        a$get()
##################################################################################################

## As described above, this function sets up the cache structure. Note the use of the "<<-"
#    assignment operator, which causes a search of the parent environments for the variable.
#    If the variable is found in a parent environment, then that definition is used, otherwise
#    it is created in the global environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mat) m <<- mat
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function attempts to get the inverse from cache.  If it is not found, the inverse
#    is computed using the "solve" function and then stored in cache for later use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
