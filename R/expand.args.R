expand.args <- function(...){
  dots <- list(...)
  max_length <- max(lengths(dots))
  lapply(dots, rep, length.out = max_length)
}