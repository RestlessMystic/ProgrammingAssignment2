# There are two functions. makeCacheMatrix function generates the functions
# that are used by the cacheSolve function. CacheSolve function checks if the
# inverse exists. If not, inverse is created.




# MakeCacheMatrix returns a list of functions
#		set : Sets value to the matrix to the variable x
#		get : Returns the value of the matrix
#		setinv : Sets the inverse of the matrix to the variable inv
#		getinv : Returns the inverse of the matrix, if it exists. Else NULL
#

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solveinv) inv <<- solveinv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#cacheSolve checks for existence of inverse
#		If inverse is already calculated, returns that value of inverse.
# 		Else, Calculates inverse and saves it in the environment, and returns it.
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
