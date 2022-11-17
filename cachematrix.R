## The first function, makeCacheMatrix creates a special "Matrix",
##which is really a list containing a function to

##1 set the value of the Matrix

##2 get the value of the Matrix

##3 set the value of the inverse Matrix

##4 get the value of the inverse Matrix


makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y){
    x<<-y
    minv<<-NULL
  }

get <- function() x
setinv <- function(solve) minv <<- solve
getinv <- function() minv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## CacheSolve will return the inverse of a matrix using 
##the makeCacheMatrix values, if the matrix is unchanged, 
##the inverse from the cache will be returned

cacheSolve <- function(x, ...) {
  minv <-x$getinv()
  if(!is.null(minv)){
    message("getting cached data")
    return(minv)
  }
  data <-x$get()
  minv<-solve(data, ...)
  x$setinv(minv)
  minv
        ## Return a matrix that is the inverse of 'x'
}
