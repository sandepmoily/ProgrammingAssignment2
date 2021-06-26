## Generate inverse of a given matrix

## The first function is "makeCacheMatrix" to return a list of functions
## 1. set value of the matrix
## 2. get value of the matrix
## 3. Inverse the matrix using solve() function
## 4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }

  get <- function(){x}
  
  setinv <- function(inverse) {
    a <<- inverse
  }
  
  getinv <- function(){a}
  
  list (set = set, get= get, setinv = setinv, getinv =getinv)
  
}


## Below function is to calculate the inverse of the matrix created with the above function.
##It will furst checks to see if the inverse has already been claculated,
## in which case it will return the inverse from cache. If not, 
##it calculates the inverse of the matrix and sets the value of the inverse
##in cache through setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    a <- x$getinv()
    if(!is.null(a)) {
      message("Getting from cache")
      return(a)
    }
    
    data <- x$get()
    a <- solve(data, ...) ## calculate the inverse
    x$setinv(a)
    a
    
}
