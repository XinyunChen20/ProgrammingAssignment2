## This script contains two functions that create a list to store a matrix
## and caches its inverse.

## The first function, makeCacheMatrix, creates a special "matrix", which  
## is actually a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    s<- NULL
    set<- function(y){
        x<<- y
        s<<- NULL
    }
    get<- function() x
    setsolve <- function(solve) s<<-solve
    getsolve <- function() s
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. It first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setsolve 
## function.

cacheSolve <- function(x, ...) {
    s<- x$getsolve()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ... )
    x$setsolve(s)
    s
}
