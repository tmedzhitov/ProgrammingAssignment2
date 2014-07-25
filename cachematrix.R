## Below are two functions that are used to create a special object that stores a numeric vector and caches its inverse value.
## Example of using them:
## m1 <- makeCacheMatrix(matrix(rnorm(100) * 34.5, 10, 10))
## This will calculate the inverse as it is the first time we call  cacheSolve()
## m2inv <- cacheSolve(m1) 
## The second call to cacheSolve() will be faster as it returns the cached inverse matrix 
## m3inv <- cacheSolve(m1)


## makeCacheMatrix function creates a special "vector", which is really a list containing a function to:
## 
## set the value of the vector
## get the value of the vector
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## define a finction that sets the value of the return vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## define a function that returns the value of the vector to the caller
  get <- function() x
  ## define get/set for inverse matrix, same as above
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  ## retun the list of 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The following function calculates the inverse of the special "vector" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## Check if the inverse matrix has been cached before or not
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## get the matrix to solve
  data <- x$get()
  ## call the solve() function to find inverse matrix
  m <- solve(data, ...)
  ## save it in the cache
  x$setinverse(m)
  ## return the found inverse matrix
  m
}



