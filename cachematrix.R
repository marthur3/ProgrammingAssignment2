## These functions will allow square matrix inversion solutions to be cached 

## makeCachematrix is a function to create a list of functions to store a matrix inversion

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set <- function(y) {
                x<<- y
                m<<- NULL
        }
        get <- function()x
        setmatrix<- function(matrix) m<<- matrix
        getmatrix<- function()m
        list(set = set, get=get, setmatrix = setmatrix, 
             getmatrix= getmatrix)

}

## cacheSolve calculates the inverse of a matrix, if the solution is already cached it returns that instead

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        m<- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m<- solve(data,..)
        x$setmatrix(m)
        m
}
