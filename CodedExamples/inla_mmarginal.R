#' A function to obtain posterior modes of marginals from an inla object
#'
#' This function allows you to get the posterior mode of variances from a fitted inla object. This is useful because the output of inla is stored as precisions.
#' @param r.out Fitted inla() object
#' @keywords inla
#' @export
#' @examples
#' inla_mmarginal()

inla_mmarginal <- function(r.out){ 
results <- sapply(r.out$marginals.hyperpar, 
       function(y) 
         inla.mmarginal(inla.tmarginal(function(x) 1/x, y)))

names(results) <- sapply(as.vector(as.character(names(results))), function(y) gsub("Precision", x=y, "Mode of variance"))
  results
}

