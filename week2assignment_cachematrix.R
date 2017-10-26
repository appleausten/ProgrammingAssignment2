makeCacheMatrix <- function(x = matrix()) {   #set the value of the matrix
        m <- NULL
        set <- function(y){ #set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x #get the value of the matrix
        setinverse <- function(solve) m <<- solve #set the value of inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        
}

cacheSolve <- function(x, ...){ 
        m <- x$getinverse()
        if(!is.null(m)) {  #checking whether the matrix has been inverted 
                message("getting cached data")
                return(m) #if yes, retun inverse matrix
        } 
        data <- x$get() #if no, do the inverse by getting back the matrix
        m <- solve(data, ...) # do the inverse
        x$setinverse(m) #set the inverse
        m #return the inverse matrix
}