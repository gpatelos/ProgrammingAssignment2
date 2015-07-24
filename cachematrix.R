## This function creates a special "matrix" object that can cache its inverse as
## part of an R programming course. For more information see https://www.coursera.org/course/rprog

## makeCacheMatrix is a function that stores a list of functions
## set changes the matrix in the main function
## get returns the matrix in the main function
## setinverse stores the input value of the "inverse" variable in the main function
## getinverse returns the matrix inverse in the main function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve calculates returns the inverse of the matrix, if neccessary. Before calculating,
## it cleverly uses R's lexical scoping ability to check to see if the inverse has already been 
## performed.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
        
}
