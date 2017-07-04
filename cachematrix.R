## Functions to get the Inverse of a matrix, but which caches the result
## There will be 2 functions, 1 (makeCacheMatrix) to first create the object that will store the data and the cached value
## The 2nd function (cacheSolve) uses the object return from makeCacheMatrix, and will retrieve the stored value in case it already exists

## This function takes a matrix as argument, and stores in through the set defined in the function
## The <<-operator is used to make sure that the assignment can be done outside of the defining function, and is therefore also accessible outside of it
## It returns a list with 4 functions, getter and setter for the data and the calculated inverse

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set <- function (y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function () x
        setInverse <- function(inverse) m_inverse <<- inverse
        getInverse <- function() m_inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates and returns the inverse of a matrix (by using the solve function)
## It expects as argument an object created by the makeCacheMatrix function
## If the inverse is already calculated, and therefore stored on the object, it is obtained from there, otherwise it is calculated with the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$getInverse()
        if (!is.null(m_inverse)) {
                message("getting cached inverse of the matrix")
                return(m_inverse)
        }
        data <- x$get()
        m_inverse <- solve(data)
        x$setInverse(m_inverse)
        m_inverse
}
