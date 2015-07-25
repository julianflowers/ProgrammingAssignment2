## I think this is what is happening. The first function takes a (square) matrix as input and calculates the inverse and stores it
## for example 
##> mymatrix<- matrix(1:4,2,2)
##> b<-makeCacheMatrix(mymatrix)
##> b$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##The second takes the output and returns the inverse if it exists
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## We can check this by 
##> c<-b$get()
##> d<-cacheSolve(b)
##getting cached data
##> c %*% d
##      [,1] [,2]
##[1,]    1    0
##[2,]    0    1
## which gives the identity matrix



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
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



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
