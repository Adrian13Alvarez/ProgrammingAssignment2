## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL             #initialize the matrix that will return the inverse
        
        set <- function(y)      #Defines the set function,
        {                       #this function assigns new value of matrix
                x <<- y
                m <<- NULL      #In case new matrix, reset inv to NULL
        }
       
        get <- function() {x}  #Returns the value of the matrix
        
        setinverse <- function(inverse) inv <<- inverse  ## Sets the value of the invertible matrix        
        
        getinverse <- function() inv                     ## gets the value of inv where called 
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## Returns a list that will be called with the $ operator
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) 
        {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
