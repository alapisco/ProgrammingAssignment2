## Put comments here that give an overall description of what your
## functions do

#This code is used to create a special matrix that has its inverse stored in cache  so its not 
# neccesary to recalculte its inverse. It is assumed that the matrix supplied is always invertible

# Example
# > m1 <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
# > m <- makeCacheMatrix(m1)
# > cacheSolve(m)
#     [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

## This function creates a special type of matrix

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL 			# Initialize the 'inverse' variable to NULL;
	set <- function(y) {		# 'y' in this case is the matrix arg passed
                                        # into 'makeCacheMatrix'
                x <<- y
                inv <<- NULL
        }
        get <- function() x		# Create a function 'get' in the 'makeCacheMatrix'
                                      

	setinv <- function(new_inv){  	# Takes a value ('new_inv') and sets it to the
                                        # value of 'inv' in the 'makeCacheMatrix' frame.
        	inv <<- new_inv
        }
        
        getinv <- function() {		# returns the value of 'inv' from the
                                        # 'makeCacheMatrix' frame.
        	return(inv)
        }
        
        
        
        list(set = set,get = get, setinv = setinv, getinv = getinv) 	 # Lists out the values of the functions in the
       setmean = setmean,              					 # makeVector frame.

}


## This function calculates the inverse of the prevous "special" matrix .It first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        local_inv <- x$getinv()			 # Goes to the'x' environment and assigns the 
                                       		 # 'INV' value from that environment to this one.
        
        if(!is.null(local_inv)) {			# If the 'x' environment has been evaluated 
                                      			  # before, the function prints the message and
                                      			  # the value of m (the cached mean).
                message("getting cached data")
                return(local_inv)
        }
        else {						# If this particular 'x' has never been
                                       			 # evaluted before, pull the x-matrix into a
                                       			 # local varaible called 'local_data'.
            local_data <- x$get()
            local_inv <- solve(local_data)               #This line calculates the inverse of the pulled matrix
        
            x$setinv(local_inv)                          # Assign the calculated inverse to the 'x'
                                       			 # environment using the 'setinv' function.
            return(local_inv) 				 # display matriz inverse
        }
}
