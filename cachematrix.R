

#code produces a cashed matrix and allows to find inverse
#what it does: creates matrix and store as inverse in cachematrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)

}

#2)
#what it does: calcs the inverse matrix from from the cache above

cacheSolve <- function(x, ...) {
        ## return matrix that is inverse of x
  
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
  }
