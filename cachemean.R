setwd("/home/malte/Git/ProgrammingAssignment2")

# Function that creates the closure which contains the cached mean
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
# Function that uses cached mean or that calculates if no cache is available
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
# Tests
vector <- makeVector(c(2, 3, 6, 4, 5))
cachemean(vector)
cachemean(vector)
mean(c(2, 3, 6, 4, 5))
