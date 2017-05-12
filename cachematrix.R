

# makeCacheMatrix does 5 steps. Sets, then subsequently gets the matrix. Sets, then subsequently gets the matrix inverse. Finally forms a list.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Start at null setting
  set <- function(y) { # Sets the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x # Gets the matrix
  setinverse <- function(inverse) inv <<- inverse # Computes inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # Change to list format
}


## cacheSolve returns the inverse of a matrix. It first checks to see if the inverse exists, and retuns it if so. If not, it computes and returns the inverse.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) { # Checks if inverse exists
    return(inv)
  }
  data <- x$get() # If not, gets the matrix
  inv <- solve(data) # Compute inverse
  x$setinverse(inv)
  inv
}

