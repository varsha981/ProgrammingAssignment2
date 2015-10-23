## makeCacheMatrix and cacheSolve are pair of functions which will help save the computation of calculating the inverse of matrix x incase its already cached.

## This function creates all the variables and functions to cache the value of inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   #initialize inverse
    set <- function(y=matrix()) {
        x <<- y    #set value of x
        inv <<- NULL #set value of inv
    }
    get <- function() x   #search for x
    setinv <- function(minv) inv <<-minv #search for inv
    getinv <-function() inv
    list(set = set(x), get = get(),
    setinv = setinv(inv),
    getinv = getinv())

}


## This function checks for the value of inverse of matrix x and retrives its value if available or calaculates if not available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #check the value for inv matrix
        inv <- getinv()
        if(!is.null(inv))
        {
            message("getting cached data")
            return(inv)
        }
        data<-get()
        inv <-solve(data)
        setinv(inv)
        inv
}
