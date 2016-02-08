## Question -> Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a private inverse object to hold onto the equivalent inverse value

makeCacheMatrix <- function(mat = matrix()) {
  
  inverse <- NULL
  set <- function(x) {
    message(x)
    mat <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mat);
  setInverse <- function(inv) {inverse <<- inv};
  getInverse <- function() return(inverse);
  hasBeenInverted <- function() !is.null(inverse);
  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse,hasBeenInverted=hasBeenInverted))
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(fn, ...) {
  inverse <- fn$getInverse()
  if(fn$hasBeenInverted()) {
    message("Getting cached data...")
    return(inverse)
  }
 
  message("Inverting")
  data <- fn$get()
  inverse <- solve(data, ...)
  fn$setInverse(inverse)
  return(inverse)
}

# testing below

start <- function(){
  cat("\014")
}

# test data
testMatrix = matrix(c(4,2,2,1,2,6,3,4,2),   # the data elements 
                    nrow=3,                           # number of rows 
                    ncol=3,                           # number of columns 
                  byrow = TRUE)                     # fill matrix by rows 


start()
#debug(cacheSolve)
#debug(makeCacheMatrix)
eval <- makeCacheMatrix(testMatrix)
eval$set(testMatrix)

for(i in 1:10)
{
  cacheSolve(fn = eval, testMatrix)
}
print(i)


