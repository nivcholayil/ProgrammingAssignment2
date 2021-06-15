## Here we are creating two functions which will work together to 
## produce our desired output, i.e. the inverse of a given matrix. 


## The first function is "makeCacheMatrix" which returns a **list** of function 
## 1. setting the value of the matrix, 
## 2. getting the value of the matrix, setting
## 3. the inverse of the matrix using the solve() function, and 
## 4. getting the inverse of the function. 


makeCacheMatrix <- function(x = matrix()){
    a <- NULL
    set <- function(y){
        x <<- y
        a <<- NULL
    }
    get <- function() {x}
    setinv <- function(inverse) {a <<- inverse}
    getinv <- function() {a} # function to obtain inverse of the matrix
    list(set=set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## The second function calculates the inverse of the matrix created with 
## the above function. it first checks to see if the inverse has already been 
## calculated, in which case it will return that inverse. If not, it will 
## it calculates the inverse of the matrix and sets the value of the inverse 
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    a <- x$getinv()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)  # calculates inverse value
    x$setinv(a)
    a
}
