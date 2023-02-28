#' Solve 0-1 Knapsack Problem #'
#' Given the three parameters; wt, vt, w, the function create the matrix. Each cell includes the best value given some restraints
#' @param w represents the weight limit of Knapsack. Total weight of items inside the knapsack cannot exceed this value
#' @param wt represents each item's weight
#' @param vt represents each item's value
#' @return the maximum value in the matrix
#' @export

knapsack <- function(wt, vt, w) {
    n <- length(wt)
    M <- matrix(0, nrow = n + 1, ncol = w)
    # to skip the first row, start with 2 to n+1
    for (i in 2:(n+1)) {
        for (j in 1:w) {
    # if the weight of new item is over j-th, use the value at {(i-1), j}
            if (j < wt[i - 1]) {
                M[i, j] <- M[i - 1, j]
            }
    # ifelse, comparing m[i-1,w] & m[i-1,w-wi]+v and pick the bigger one
        else {
            M[i, j] <- max(M[i - 1, j], M[i - 1, j - wt[i - 1]] + vt[i - 1])
            }
        }
    }
    # change the row names
    rownames(M) <- 0:3
    # get the maximum value in the matrix
    M <- max(M)
    return(M)
}

