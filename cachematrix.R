## My two functions are used to create a special matrix that stores a special matrix
## and cache's its inverse through the R command solve.


## The first function, makeCacheVector creates a special "matrixr", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix through the solve R command
## get the value of the inverse matrix through the solve R command

makeCacheMatrix <- function(x = matrix()) {
  m <- "There is not value for the inverse matrix yet" 
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse matrix of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse matrix
## has already been calculated. If so, it gets the inverse matrix from the cache 
## and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and 
## sets the value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Below there is an example of how these functions work.

x <- rbind(c(1, 4), c(2, 3)) ## it attributes a value matrix to x variable
class(x) ## it shows the class of x variable which will be a matrix
xx <- makeCacheMatrix(x) ## it gets a list of the functios above allocating them in xx variable
xx$get() ## it gets the x matrix
xx$getinverse() ## if the inverse matrix has not already been calculated it prints the m value "There is not value for the inverse matrix yet"
xx$setinverse(solve(x)) ## it sets the inverse matrix calculating its.
xx$getinverse() ## it prints the inverse matrix.
## In addition, after anybody else has done the above commands if they call xx$getinverse() again
## it prints the inverse matrix because it has already been calculated.
