## The assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## It is assumes that x is a square invertible vector
        ## The function returns a list with functions to 
        ## 1. set the value of the list
        ## 2. get the value of the list
        ## 3. calculate the inverse of x
        ## 4. get the inverse of x
        
        inv <- NULL ## we are setting the value inv to NULL as a placeholder for a future value
        
        set <- function(y) {
                ## This function just sets the vector x to the new vector y
                ## and resets inv to null
                ## the values are assigned to objects outside the current environment
                x <<- y
                inv <<- NULL
        }
        
        
        get <- function() x ## returns the vector x
        setinv <- function(inverse) inv <<- inverse ## sets inv to the inversed matrix, inverse
        getinv <- function() inv ## returns the matrix, inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) ## returs the list of the functions defined above

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv() ## inv is assigned the value of x via the getfunction
        
        ## Checking if inv has already been calculated, then we should use the cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If it was not cached we must caclulate it
        data <- x$get() ## the variable data is assigned the value of x via the getfunction
        inv <- solve(data, ...) ## The inverse is calculated via the solve()-function
        x$setinv(inv)  # the value of the inverse is set in the cache via the setinv function.
        inv
        
}
