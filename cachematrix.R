## makeCacheMatrix and cacheSolve calculate and return the inverse of a square invertible matrix
## this solution assumes that any matrix provided will be invertible and square
## the purpose of this solution is to potentially save costly computing by using caching

## makeCacheMatrix creates a 'matrix object' that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL #initializes variable to hold inverse of matrix x
  
  set<-function(y){ 
    x<<-y 
    i<<-NULL 
  }
  
  get<-function() x
  
  setinverse <- function(solve) i<<-solve
  
  getinverse <- function() i
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computes inverse of matrix returned by makeCacheMatrix
## will retrieve inverse from cache if it has already been calculated, and has not changed

cacheSolve <- function(x, ...) {
       
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
}


#testing script
a<-makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2))
a$get()
a$getinverse()
cacheSolve(a)
