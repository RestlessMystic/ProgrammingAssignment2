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
        set <- function(y) {			#Setting Values of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x				# Returns the matrix
        setinv <- function(solveinv) inv <<- solveinv	# Sets inverse as the
														# passed value
        getinv <- function() inv		# Returns the inverse				
        list(set = set, get = get,		# Return the list containing these
             setinv = setinv,			# functions
             getinv = getinv)
}


#cacheSolve checks for existence of inverse
#		If inverse is already calculated, returns that value of inverse.
# 		Else, Calculates inverse and saves it in the environment, and returns it.
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv <- x$getinv()				# Getting Inverse	
        if(!is.null(inv)) {				# Inverse is already cached
                message("getting cached data")
                return(inv)
        }
        data <- x$get()					# Getting data
        inv <- solve(data)				# Finding Inverse
        x$setinv(inv)					# Setting Inverse
        inv
}
