## makeCacheMatrix takes a numeric matrix, saved in 'matrix_data' and returns 
## a list which enables underlying functions to be called as 'matrix_data$funtion_name()'
makeCacheMatrix <- function(matrix_data = matrix()) {
        ## Initialize the inverse matrix container.
        matrix_data_inverse <- NULL
		## matrix_data$set(numeric_matrix_arg): sets the value for matrix_data and nullifies its inverse
		## matrix_data$get: returns matrix data to cacheSolve()
		## matrix_data$setInverse: sets the value for matrix_data_inverse, which is calculate in cacheSolve()
		## matrix_data$getInverse: returns the inverse matrix if it is already calculated
		set <- function(set_matrix_data) {
		        matrix_data <<- set_matrix_data
				matrix_data_inverse <<- NULL
		}
		get <- function() matrix_data
		setInverse <- function(cached_matrix_data_inverse) matrix_data_inverse <<- cached_matrix_data_inverse
		getInverse <- function() matrix_data_inverse
		list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve takes a numeric matrix, saved in matrix_data and returns the computed or cached matrix
cacheSolve <- function(matrix_data, ...) {
        ## Return a matrix that is the inverse of 'matrix_data'
		matrix_data_inverse <- matrix_data$getInverse()
		## Checks for NULL. If true returns cached inverse matrix else proceeds to inverse calculation
		if(!is.null(matrix_data_inverse)) {
		        message("getting cached inverse matrix data")
				return(matrix_data_inverse)
		}
		## If the control reaches till here, either the inverse was not calculated or a new matrix was set
		data <- matrix_data$get()
		matrix_data_inverse <- solve(data)
		matrix_data$setInverse(matrix_data_inverse)
		matrix_data_inverse
}