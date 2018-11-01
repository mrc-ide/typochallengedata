compute_freq_error_per_contribution <- function(id) {
  table(ret[ret$id %in% id,]$correct)['FALSE'] / 
    sum(table(ret[ret$id %in% id,]$correct))
}