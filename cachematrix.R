## Put comments here that give an overall description of what your
## functions do:

### this pair of functions caches the calculated inverse of a matrix.

## Write a short comment describing this function:

### This function returns a list of functions that can: 1. create a matrix
### or set the matrix to a new matrix; 2. retrieve the matrix; 3. set the 
### inverse of the matrix as calculated from second function; 4. retrieve
### the inverse.

makeCacheMatrix <- function(x = matrix()) {    ## input is set as a matrix
    In <- NULL    ## set In for future use
    set <- function(y) {    
        x <<- y    ## set x to a new matrix y
        In <<- NULL    ## so the inverse has to be resetted
    }
    get <- function() x    ## to see the current x
    setinverse <- function(inverse) In <<- inverse    ## stores the In from line 39 & 40
    getinverse <- function() In    ## to see the current In
    list(set = set, get = get,    ## later, call any of the function on x
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

### This function calculate the inverse of a given matrix, returns it, and
### caches the result by calling the first function.

cacheSolve <- function(x, ...) {
    In <- x$getinverse()    ## get the In from last function
    if (!is.null(In)) {    ## check if In is computed already
        message("getting cached data")
        In    ## returns In from cached data
    } else {    ## if In is NULL
        data <- x$get()    ## the data is obtained by calling get() on x
        In <- solve(data, ...)    ## the inverse is calculated
        x$setinverse(In)    ## In is stored in cache using line 20
        In    ## return the inverse
    }
}

## Testing:
mymatrix <- makeCacheMatrix(matrix(rnorm(900, 50, 10), 30, 30))    ## I've created a random big matrix
cacheSolve(mymatrix)    ## you can see the inverse is calculated
mymatrix$getinverse()   ## you can see my inverse is cached
cacheSolve(mymatrix)    ## you can see the message

mymatrix$set(matrix(1:4, 2, 2))    ## now I change mymatrix
mymatrix$getinverse()    ## the inverse is yet to be calculated, because "In <<- NULL"
cacheSolve(mymatrix)    ## you can see the inverse is calculated
mymatrix$getinverse()   ## you can see my inverse is cached
cacheSolve(mymatrix)    ## you can see the message
## I didn't really use setinverse(), but you can set the inverse as a known value

## Thanks for your time and effort grading my work. Wish you a lovely day:)

