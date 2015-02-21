## Since matrix inversion is usually a costly computation
## The functions bellow will serve to caching the inverse of a matrix 
## rather than computing it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Reset the value of "solved" to clear any cached data
        ## when a new Matrix is created
        solved <- NULL
        
        ## Creating a variable that returns a function for set the value of the matrix
        ## and setting new values for new matrices
        set <- function(y) {
                ## Set new value for x with the new matrix
                x <<- y
                ## Clear cache when a new matrix is created using the set function
                solved <<- NULL
        }
        
        ## Creating a variable that returns a function for get the value of the matrix
        get <- function() x
        
        ## Creating a variable that returns a function for set the value of the solve function
        setsolve <- function(solve) solved <<- solve
        
        ## Creating a variable that returns a function for get the value of the solve function
        getsolve <- function() solved
        
        ## Returning all functions created above in a list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Set the solved variable using getsolve from makeCacheMatrix to 
        ## returning the value of the solve from cache
        solved <- x$getsolve()
        
        ## If the value returned from cache was not null, 
        ## then we return this cached value to the user
        if(!is.null(solved)) {
                ## Message the user that the data are from "cache"
                message("getting cached data")
                
                ## Return the cached data
                return(solved)
        }
        
        ## Get the matrix using makeCacheMatrix and assign to the "data" variable
        data <- x$get()
        
        ## Inverse the matrix(data) using the function "solve" from R base package
        ## and store the solved data into the "solved" variable
        solved <- solve(data, ...)
        
        ## Store/Cache the result of inverse matrix using setsolve from makeCacheMatrix
        x$setsolve(solved)
        
        ## Return the solved matrix that is the inverse of 'x'
        solved                
}

