## The two functions are used for caching the inverse of a matrix whose contents are not changing.
## It is useful to cache the matrix inverse if it is required repeatedly (say in a loop), so when
## it is required again, the computation is not repeated
 
## makeCacheMatrix: This function creates an special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y=matrix()) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above using the Solve() function.
## If the inverse has already been calculated (and the matrix has not changed) then the function returns the inverse cached in the special matrix object created by the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!all(is.na(inv))) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
