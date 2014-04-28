## makeCacheMatrix takes a matrix and returns a list.
## The list has four functions, "set", "get", "setInv" and "getInv".
## The inverse matrix is stored in "setInv"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ##Must initialize the "m" as null
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve takes a matrix x and returns the inverse of x
## If the inverse has already been stored, the function pulls it
## If the inverse hasn't yet been calculated, this function calculates it

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)){   ##Check for inverse already in cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()   
  m <- solve(data,...)  ##Calculate the inverse, store in m
  x$setInv(m)
  m
        
}
