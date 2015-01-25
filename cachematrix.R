## makeCahcheMatrix is a function to creates a special "matrix" object that can cache its inverse 
## get the value of matrix
## set the value of matrix
## set the vlaue of matrix inverse 
## get the value of matrix inverse
makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##cacheSolve - this function calculates the inverse of the matrix returned by makeCahcheMatrix function.
## If the matrix hasn't changed the inverse is returned from the cache

cacheSolve <- function(x, ...) 
{
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("retrieve cached data")
                print(inv)
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}