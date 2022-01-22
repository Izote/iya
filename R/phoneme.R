new_phoneme <- function(x = list()) {
  structure(
    x,
    class = "phoneme"
  )
}

validate_phoneme <- function(x) {
  stopifnot(is.list(x))
}
