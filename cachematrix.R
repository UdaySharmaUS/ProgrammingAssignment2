# Create a special "matrix" object that can cache its inverse.
# This function takes an initial matrix 'x' as input and returns a list
# containing the matrix 'x' and a function to calculate and cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cached inverse as NULL
  cache <- NULL
  
  # Function to set the matrix and cache its inverse
  set <- function(y) {
    x <<- y  # Assign the new matrix to 'x'
    cache <<- NULL  # Reset the cached inverse
  }
  
  # Function to retrieve the cached inverse, or calculate it if necessary
  get <- function() {
    # If the cached inverse is NULL, calculate it using solve()
    if(is.null(cache)) {
      cat("Calculating inverse...\n")
      cache <- solve(x)
    } else {
      cat("Using cached inverse...\n")
    }
    # Return the cached inverse
    cache
  }
  
  # Return a list of the functions
  list(set = set, get = get)
}

# Compute the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then this function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse from the matrix 'x'
  inv <- x$get()
  # Return the cached or calculated inverse
  inv
}

