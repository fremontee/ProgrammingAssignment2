## function makeCacheMatrix takes a matris as input
## and returns a list of four functions
## setmatrix: sets the element of the list as matrix
## getmatrix: gets the matrix parameter
## getinverse: computes the inverse using solve
## setinverse: sets the computed value of inverse matrix


## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) 
{
        invmat <- NULL
        setmatrix <- function(y) 
        {
                x <<- y
                invmat <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) invmat <<- inverse
        getinverse <- function() invmat
        list(setmatrix = setmatrix, 
             getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function cacheSolve applies the core formula for matrix inversion
## after checking if the inverse has been previously computed or not
## if computed previously, it returns the cached value

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinverse()
        if(!is.null(invmat)) 
        {
                message("getting previouly cached matrix inverse data...")
                return(invmat)
        }
        message("computing fresh matrix inverse data...")
        data <- x$getmatrix()
        invmat <- solve(data) ## this is core inversion formula
        x$setinverse(invmat)
        invmat
}

## call usage for testing
## df <- matrix (c(2,1,3,1),2,2)
## dfi <- makeCacheMatrix(df)
## dfi$getmatrix()
## cacheSolve(dfi) 
## dfi$getinverse()
## dfi$getinverse()
## expected_output <- matrix (c(-1,1,3,-2),2,2)

