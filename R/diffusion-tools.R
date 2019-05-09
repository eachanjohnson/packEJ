#' Get distance of k nearest neighbor
#'
#' @param x Matrix of distances
#' @param k K-th neighbor
#' @param cutoff Maximum distance to return
#'
#' @return 1-length numeric vector
#'
#' @export
#'
get_k_nn_dist <- function(x, k, cutoff=Inf)
  min(min(cutoff, sort(x)[3 * k + 1]), sort(x)[k + 1])

#' Gaussian kernel of matrix
#'
#' @param x Matrix of distances
#' @param sigma Kernel width
#' @param alpha Kernel power
#'
#' @return Matrix
#'
#' @export
#'
gkernel <- function(x, sigma=1, alpha=2)
  exp(-(x / sigma)^alpha)

#' Adaptive Gaussian kernel of matrix
#'
#' @param x Matrix of distances
#' @param k K-th neighbor distances (used for kernal width)
#' @param cutoff Maximum kernel width
#' @param alpha Kernel power
#'
#' @return Matrix
#'
#' @export
#'
adaptive_gkernel <- function(x, k=2, cutoff=Inf, alpha=2)
  vapply(x, gkernel, 1,
         sigma=get_k_nn_dist(x, k=k, cutoff=cutoff),
         alpha=alpha)

#' Calculate affinity matrix
#'
#' Uses adaptive Gaussian kernel.
#'
#' @param m Matrix of distances
#' @param k K-th neighbor distances (used for kernal width)
#' @param cutoff Maximum kernel width
#' @param alpha Kernel power
#'
#' @return Matrix
#'
#' @export
#'
affinity <- function(m, k=2, cutoff=Inf, alpha=2)
  t(apply(m, 1, adaptive_gkernel, k=k, cutoff=cutoff, alpha=alpha))

#' Make symmetric matrix
#'
#' @param m Square matrix
#'
#' @return Square symmetric matrix
#'
#' @export
#'
symmetrize <- function(m)
  m + t(m)

#' Normalize matrix rows
#'
#' @param m Matrix
#' @param rowfun Function to calculate normalization denominator
#'
#' @return Matrix
#'
#' @export
#'
row_normalize <- function(m, rowfun=rowSums)
  t(t(m) / rowfun(m))

#' Calculate Markov matrix
#'
#' @param m Matrix
#' @param k K-th neighbor distances (used for kernal width)
#' @param cutoff Maximum kernel width
#' @param alpha Kernel power
#' @param distfun Function to calculate distnace between rows
#'
#' @return Row-normalized Markov matrix
#'
#' @export
#'
make_transition_matrix <- function(m, k=2, npcs=1, cutoff=Inf, alpha=2,
                                   distfun=function(x)
                                     parallelDist::parDist(x, threads=system('nproc') - 1)) {

  pca <- m %>% prcomp()

  frac_var <- sapply(seq_along(pca$sdev), function(x) Reduce(sum, pca$sdev[seq_len(x)])) / sum(pca$sdev)

  if (npcs <= 1) {

    npcs_ <- max(which(frac_var <= npcs))

    if ( is.infinite(npcs_) ) npcs_ <- 1

    println('Keeping', npcs_, 'PCs with total variance', npcs, '...')

  } else {

    npcs_ <- npcs
    println('Keeping', npcs_, 'PCs with total variance', sum(frac_var[seq_len(npcs_)]), '...')

  }

  println('Calculating distance...')
  dist_mat <- pca$x[ , seq_len(npcs_)] %>% distfun() %>% as.matrix()

  println('Applying adaptive Gaussian kernel (k =', k, ', alpha = ', alpha, ', cutoff = ', cutoff, ')...')
  affinity_mat <- affinity(dist_mat, k=k, cutoff=cutoff, alpha=alpha)

  println('Symmetrizing and normalizing...')
  markov_mat <- affinity_mat %>% symmetrize() %>% row_normalize()

  return ( markov_mat )

}
