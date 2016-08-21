### The following functions make it easier to compute time consuming matrix inverses
### by allowing us to cache the results of inverse computations to avoid us doing these
### numerous times. By caching the results we can simply use these results without
### having to do the calculation each time.

### Our first function uses a matrix argument which is assumed to be invertible, and sets up
### a list which sets the matrix, gets the matrix, sets the inverse of the matrix and gets
### the inverse of the matrix. This will cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y                       ### Note the use of '<<-' to assign a value to an object
    inv <<- NULL                  ### in an environment which is different from the current one.
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



### The following function returns the inverse of the matrix. It will first check if the 
### computation has been cached, this step is highlighted below, and if it has been it will
### use this result. Otherwise it will compute the inverse, cache it and then return it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){                ### This is the part which checks if the 
    message("getting cached data")  ### result has already been cached.
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


### Below are two simple 2x2 examples to illustrate how the code works. Notice that
### there is no "getting cached data" message displayed the first time the cacheSolve
### function is run in each example. This is because the computation has not been carried out
### before and so there is no cached data.

a<-matrix(1:4,2)
a
n<-makeCacheMatrix(a)
n$get()
cacheSolve(n)
cacheSolve(n)


x<-rbind(c(100,-0.5),c(-5,20))
x
m<-makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)


