#' @title Principal Component Analysis (PCA) Approximation
#'
#' @description This function performs Principal Component Analysis (PCA) on the input data `x`, extracts the specified number of principal components 'npc', and uses them to reconstruct an approximation of the original data. The approximation is then rescaled and centered to match the scale and centering of the original data.
#'
#' @param x A matrix or data frame containing the original data.
#' @param npc An integer specifying the number of principal components to use for the approximation.
#'
#' @return A matrix containing the approximation of the original data based on the specified number of principal components.
#'
#' @importFrom stats prcomp
#'
#' @export
#'
#' @examples
#' # Example usage:
#' # Generate example data
#' set.seed(123)
#' x = matrix(rnorm(100), ncol = 10)
#' # Perform PCA approximation with 3 principal components
#' approx_data = pcApprox(x, npc = 3)
#'

pcApprox = function(x, npc) {
  # Perform PCA
  pca = stats::prcomp(x)

  # Extract 'npc' number of principal components from 'pca' starting from 1
  approx_pc = pca$x[, 1:npc]

  # Use selected principal components to reconstruct approximation
  approx_data = approx_pc %*% t(pca$rotation[, 1:npc])

  # Rescale to original
  approx_data = approx_data * sqrt(pca$sdev[1:npc])
  # Recenter to original
  approx_data = approx_data + colMeans(x)

  return(approx_data)
}
