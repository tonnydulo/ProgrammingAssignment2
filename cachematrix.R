#makecachematrix function creates a matrix which is a list 
#containing a function that
#1. sets the value of matrix
#2. gets the value of the matrix
#3. gets the value of the inverse of the matrix
#4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
 i<- NULL
 set <- function(y) {
   x<<- y
                i<<- NULL
                }
              get <- function()x
                setinverse <- function(inverse)i<<- inverse
              getinverse<- function()i
              list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

cachesolve <- function(x, ...){
  #Return a matrix that is the inverse of 'x'
  i<- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
}