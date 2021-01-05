tpm = function (counts, effective_lengths) {
  rate = log(counts) - log(effective_lengths)
  exp(rate - log(sum(exp(rate))) + log(1E6))
}

rpkm = function (counts, effective_lengths) {
  exp(log(counts) - log(effective_lengths) - log(sum(counts)) + log(1E9))
}


