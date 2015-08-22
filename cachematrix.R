## Following the course examples and explanation from DanieleP's Github
## These functions first create an object (a matrix) that stores a list of functions that store the input values (the input matrix) and the solution (the inverse matrix) and allow these values to be retreived. 
## The second function retrieves the stored solution (if it has been previously stored) or computes the value (if it had not be previously stored) , then returns this value (the inverse matrix).
## These functions assume matrix supplied is always invertible (as per assignment instructions)

## set () function changes the value of x in the parent function to the input value of y, and restores the value of s (the solution) to NULL
## get () returns the value of x in the parent function (as stored by the 'set' function)
## set solve () stores the computed value of 'solve' (i.e the solution/inverse matrix) as 's'
## returns the value of s (the solution stored by the 'setsolve' function)

makeCacheMatrix <- function(x = matrix()) {
  s<- NULL 
  set <- function(y) {
    x <<- y
    s <<- NULL
  } 
  get <- function() x 
  setsolve <- function(solve) s <<- solve 
  getsolve <- function() s 
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The assigned value of 's' (stored using the 'setsolve' function) is returned using the 'getsolve' function
## If the value of 's' is not NULL (i.e., it is the previously stored value), the message is printed along with the stored value of 's'.
## If 's' is NULL then the value of x (the stored input from the 'makeCacheMatrix' function) is assigned to the object 'data' using the 'get' function.
## The inverse matrix is then computed using the 'solve' function and assigned to the object 's'.
## The value of 's' is stored using the 'setsolve' function
## Then the value of 's' (the solution) is printed

cacheSolve <- function(x, ...) {
  s <- x$getsolve() 
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  } 
  data <- x$get()
  s <- solve(data, ...) 
  x$setsolve(s) 
  s 
        ## Return a matrix that is the inverse of 'x'
}
