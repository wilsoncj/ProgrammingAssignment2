## Function "cacheSolve" computes the inverse of the special matrix (which
## is the input of cachemean returned by makeCacheMatrix).
## 

## first step is to make a matrix and assign it to variable X
## initialize m to NULL

makeCacheMatrix <- function ( x = matrix ()) {
    m = NULL
    set <- function (y) {        ## if user wants to reset matrix
          x <<- y                ## reassign "new" matrix to x
          m <<- NULL             
    }
    get <- function ()x
    setinverse <- function (solve)  m <<- solve
    getinverse <- function ()m
    list ( set = set, get = get,           ## this list is used
           setinverse = setinverse,        ## as the input to cacheSolve()
           getinverse = getinverse)
}

## Return a matrix that is the inverse of "x"
## If the inverse has not been calculated data gets the matrix
## stored with makeCacheMatrix, m calculates the inverse
## and x$getinverse(m) stores it in the object m in makeCachetMatrix

cacheSolve <- function (x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){                   ## if user had calculated 
      message("getting cached data")   ## the same matrix before
      return(m)                        ## return(m) returns old results
    }
    data <- x$get()                    ## otherwise, get uncalculated matrix
    m <- solve(data, ...)              ## calculate the inverse matrix
    x$setinverse(m)                    ## reassign inverse matrix
    m                                  ## print the inverse matrix
}


