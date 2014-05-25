makeCacheMatrix <- function(x = matrix()) {
        
        ## The function is used to create a special object that stores 
        ## a matrix and cache its inverse value
        ##
        ## The function returns a list containing 4 elements:
        ## 1  set a value for a matrix
        ## 2  get the value of a matrix
        ## 3  set the value of the matrix inverse
        ## 4  get the value of the matrix inverse
        
        ## Usage: a<-makeCacheMatrix()
        ##        a$set(matrix(1:4,2,2))
        
        # Initializes the cache
        
        m <- NULL
        
        # Stores the matrix for which inverse needs to be computed
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Returns the matrix
        
        get <- function() x
        
        # Sets the inverse value in the cache
        
        setmatrix <- function(solve) m <<- solve
        
        # Gets the inverse value from the cache
        
        getmatrix <- function() m
        
        ## Returning the list containing the 4 functions
        
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
        
        ## The function calculates the inverse of the special matrix 
        ## created using the makeCacheMatrix() function. However, it 
        ## first checks to see if the inverse has already been calculated. 
        ## If so, it gets the inverse value from the cache and skips 
        ## the computation. Otherwise, it calculates the inverse of 
        ## the matrix and sets the value of the inverse in the cache 
        ## via the setmean function
        
        ## Usage: a<-makeCacheMatrix()
        ##        a$set(matrix(1:4,2,2))
        ##        cacheSolve(a)
        
        # Gets the value of the matrix
        
        m <- x$getmatrix()
        
        # Checks if the inverse is available in the cache
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Computes the inverse of the matrix using the solve function
        
        matrix <- x$get()
        m <- solve(matrix, ...)
        
        # Sets the computed inverse value in the cache and returns it
        
        x$setmatrix(m)
        m
}