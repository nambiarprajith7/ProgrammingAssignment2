## This function is written for Coursera Week 3 assignment
## Github user: nambiarprajith7
## The function written here gives the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                            
  set <- function(y) {                   
    x <<- y                             
    inv <<- NULL                        
  }
  get <- function() {x} 
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)}

## This function calculates the inverse of the matrix. 
##if the inverse has already been calculated, then it will return from cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
