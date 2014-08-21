## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # Copy the initial matrix, otherwise due to lazy argument evaluation
    # it could be modified outside like this:
    # > a <- 1:4
    # > m <- makeCacheMatrix(a)
    # > a[[1]] <- 100L
    # > m$get()
    # [1] 100   2   3   4
    # > a
    # [1] 100   2   3   4
    m <- x

    # Reserve space for inversion result
    mInversed <- NULL

    get <- function() m

    # There's no need for setInversed counterpart
    # since we can compute the inversion on-demand
    # and store the result for future reuse.
    getInversed <- function() {
        if (is.null(mInversed)) {
            mInversed <<- solve(m)
        }
        mInversed
    }

    list(
        get = get,
        getInversed = getInversed
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # Just delegate to getInversed, everything is already done there
    x$getInversed()
}
