## Together, the functions in this file calculate the inverse of a matrix, and
## store that inversed matrix so that it can quickly be retrieved in future 
## calculations without needing to re-calculate it each time.


## This function sets initial values and creates the functions necessary to set 
## and retrieve the cached matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }

     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list (set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}





## This function returns the inverse of a matrix if the inverse is cached, and 
## calculates and caches it if it's not

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)){
          message("Getting cached data")
          return(i)
     }
     
     data <- x$get()
     i <- solve(data)
     x$setinverse(i)
     i
}


