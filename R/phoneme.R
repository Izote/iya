#' Constructor for the phoneme class.
#'
#' @param x A named list containing IPA data.
#' @param class A single-element character vector - "consonant" or "vowel".
#'
#' @return A phoneme class object.
#'
new_phoneme <- function(x, class = character()) {
  structure(
    x,
    class = c(class, "phoneme")
  )
}

#' Constructor for the consonant (sub)class.
#'
#' @param x A named list containing IPA data.
#'
#' @return A consonant (sub)class object.
#'
new_consonant <- function(x) {
  new_phoneme(x, class = "consonant")
}

#' Constructor for the vowel (sub)class.
#'
#' @param x A named list containing IPA data.
#'
#' @return A vowel (sub)class object
#' @keywords internal
#'
new_vowel <- function(x) {
  new_phoneme(x, class = "vowel")
}

#' Validates a phoneme class object.
#'
#' @param x A potentially-valid phoneme class object.
#'
#' @return A valid phoneme class object.
#' @keywords internal
#'
validate_phoneme <- function(x) {
  if (!is.list(x)) {
      stop("Provided argument is not a list.")
  }

  if (!length(names(x))) {
    stop("List provided to constructor is unnamed.")
  }

  if (!all(c("representation", "articulation") %in% names(x))) {
    stop("Incorrect names provided to constructor in list.")
  }

  if (!is.character(x[["representation"]])) {
    stop("Representation provided to constructor is not a character vector.")
  }

  if (!is.character(x[["articulation"]])) {
    stop("Articulation provided to constructor is not a character vector.")
  }

  if (length(x[["articulation"]]) != 2) {
    stop("Articulation vector must be of length 2.")
  }

  x
}

#' Creates a new phoneme class object.
#'
#' @param ipa A named list referencing the phoneme's IPA representation and articulation.
#'
#' @return A phoneme class object.
#'
phoneme <- function(ipa) {
  validate_phoneme(new_phoneme(ipa))
}

#' Creates a new consonant object.
#'
#' @param ipa A named list referencing the vowel's IPA representation and
#' articulation as length 1 and 2 character vectors respectively.
#'
#' @return A consonant object.
#' @export
#'
consonant <- function(ipa) {
  validate_phoneme(new_consonant(ipa))
}

#' Title
#'
#' @param ipa A named list referencing the consonant's IPA representation and
#' articulation as length 1 and 2 character vectors respectively.
#'
#' @return A vowel object.
#' @export
#'
vowel <- function(ipa) {
  validate_phoneme(new_vowel(ipa))
}
