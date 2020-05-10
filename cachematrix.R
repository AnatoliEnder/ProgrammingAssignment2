## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(ma = matrix()){     #function with arg matrix
        x <- NULL
        set <- function(y) {                    #sets value of matrix
                ma <<- y
                x <<- NULL
        }
        get <- function() ma                    #gets value of matrix
        setinverse <- function(solve) x <<- solve       #set value of the inverse matrix
        getinverse <- function() x                      #get value of the inverse matrix
        
        list(set = set, get = get,                #returns list 
             setinverse = setinverse,            
             getinverse = getinverse)
        
        
}      


## Write a short comment describing this function

cacheSolve <- function(ma, ...) {
        x <- ma$getinverse()
        if(!is.null(x)) {                        # returns cached matrix inverse if it has been already calculated
                message("getting cached data")
                return(x)
        }
        data <- ma$get()                        # otherwise it creates the inverse of the matrix
        x <- solve(data, ...)
        ma$setinverse(x)
        x
}