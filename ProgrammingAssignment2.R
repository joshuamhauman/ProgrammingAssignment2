## Create a special object that stores a numeric vector and cache's its mean

## Creates a special "vector"

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
  }
  


##  Calculates the mean of the special "vector" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }

    
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  }
