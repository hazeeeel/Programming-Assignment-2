##This line of code creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  q <- NULL
  set <- function(w){
    x <<- w
    q <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) q <<- inverse
  getInverse <- function() q 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


cacheSolve <- function(x, ...) {
  q <- x$getInverse()
  if(!is.null(q)){
    message("getting cached data")
    return(q)
  }
  mat <- x$get()
  q <- solve(mat,...)
  x$setInverse(q)
  q
}

}