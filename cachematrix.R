## This fuction simply the potentially time-consuming computations of calculating 
## an inverse of a matrix by caching the answer. This enable calling of cached answer
## if the matrix has not changed.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # stores the cached value
        # initialize to NULL
        inv = NULL
        set = function(y) {
        # create the matrix in the working environment
                x <<- y
                inv <<- NULL
        }
        # get the value of the matrix
        get = function() x
        # invert the matrix and store in inv
        setinv = function(inverse) inv <<- inverse 
        # get the inverted matrix from inv
        getinv = function() inv
         # return the created functions to the working environment
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in inv,
## it it created in the working environment and it's inverted value
## is stored in inv

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        # return inverted matrix from cache if it exists
        # else create the matrix in working environment
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        # get the matrix in the working environment
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        
        return(inv)
}

