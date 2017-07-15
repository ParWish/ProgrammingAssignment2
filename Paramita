## This program consits of 2 functions to create a special matrix and cache it's inverse 

## One function creates a special matrix which contains a list of a matrix and some special functions.
## and the second calculates the inverse of this matrix and caches the same

## The makeCacheMatrix function creates a special matrix which is a lts of a matrix and 4 functions:
## 1. set : Sets the value of the matrix
## 2. get : Returns the matrix
## 3. setinverse : Sets the value of the inverse
## 4. getinverse : Returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )


}


## The cacheSolve function checks if the inverse exists in the cache. 
## If it exists, it returns the same
## If it does not exist, it calculates the inverse using solve function and returns the same

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)){
	 message("getting cached data")
                return(inv)
        }       
	  data <- x$get()
	  if (nrow(data) == ncol(data)){
        inv <- solve(data)
        x$setinverse(inv)
	  return(inv)
	  }
	  else
	  {
		message("Inverse does not exist")
	  }
}
