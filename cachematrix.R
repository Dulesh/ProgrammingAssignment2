## Put comments here that give an overall description of what your
## functions do

##There are two functions makeCacheMatrix,cacheSolve
##There are two functions makeCacheMatrix,cacheSolve. 
##Basically in happen calculated matrix inverse result save for future use( cache ).
##The first time we want to solve the matrix. 
##Then second-time we can get the result from the first time saved result( cache) without resolving

## Write a short comment describing this function

## makeCacheMatrix, assign function to set,get,setInverse,getInverse
makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
  set <- function(y){
  x <<- y
  i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i<<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


## Write a short comment describing this function

## This funtion use for get cache data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
  message("getting cached data")
  return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i
}
