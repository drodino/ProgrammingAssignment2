## Put comments here that give an overall description of what your
## functions do

## Per the assignment, this function is supposed to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ## Below is the only part of the code that really differs from the example code for makevector
        ## I simply just swapped out the mean function language with the inverse function language
        
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Per the assignment, this is supposed to compute the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
        
        ## This code is basically the same as the example code for the example code in the cachemean function
        ## I simply just swapped out the mean function language with the inverse function language
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        ## The code below is different then the example in that I applied the solve() function to return the inverse
        
        m <- solve(data, ...)
        x$setinverse(m)
        m    
        
}

##Steps for testing all of this:
##
##Step 1:  Pass a matrix to the makeCacheMatrix function
##Step 2:  Call my_matrix$get() to see that the matrix was initiated properly
##Step 3:  Check to see that there is no cached value by running this function:  my_matrix$getinverse()
##Step 4:  Pass the matrix used in Step 1 to the cacheSolve() function
##Step 5:  Runnign the cacheSolve() function again will return the cached values (try passing a large matrix to see the performance)
##Step 6:  Test to see if the same values are returned for the inverse as using the native function:  [matrixname]$getInverse()

##Note:  This seems to only work on square matrices of 2x2  I could not get it to work on anything larger like a 3x3 matrix


