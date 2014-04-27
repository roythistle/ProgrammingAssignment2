

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a functions to
## set the value of the matrix
## get the value of the matrix
## set the computed value of the matrix inverse, computed with solve, to the cache location
## get the computed value of the matrix inverse from the cache location
## This function can be used to cache the value of the time intensive computation if a  matrix
## inversion, so that it does not have to be calculated repeatedly, when alredy done once
## use mymatrix = makeCacheMatrix(matrix(1:4,2)) to create a 2X2 matrix.
## call the functions provided in makeCacheMatrix by $ indexing
## mymatrix$get() will print the matrix to the console
## mymatrix$getsolve() will print the inverse of mymatrix to the console

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                   #if NULL the cache is not initialized yet
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)    #a list of functions

}


## The following function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse function
## use cacheSolve(mymatrix) to print the inverse of the mymatrix to the console
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()                             #check the cache
        if(!is.null(m)) {                             #see if its initialized
                message("getting cached data")
                return(m)                             #if it is return the 
        }                                             #previous computation
        data <- x$get()                               #otherwise get the matrix
        m <- solve(data, ...)                         #compute the inverse
        x$setsolve(m)                                 #initialize the cache
        m
}
