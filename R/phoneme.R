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

  if (length(x[["articulation"]]) != 3) {
    stop("Articulation vector must be of length 3.")
  }

  x
}


#' Create a new consonant object.
#'
#' Takes IPA data, formatted as a named list of character vectors, and returns a consonant object. A consonant object is a subclass of the more general phoneme object.
#'
#' @param ipa A named list following the format `list(representation = character(), articulation = character())` with each element representing
#' the consonant's IPA representation (e.g. "a", "p", etc.) and phonetic dimensions respectively.
#'
#' @return A consonant object.
#'
#' @examples
#' ipa_data <- list(representation = "p", articulation = c("voiceless", "bilabial", "plosive"))
#' p <- consonant(ipa_data)
#'
#' @export
#'
consonant <- function(ipa) {
  validate_phoneme(new_consonant(ipa))
}


#' Create a new vowel object.
#'
#' Takes IPA data, formatted as a named list of character vectors, and returns a vowel object. A consonant object is a subclass of the more general phoneme object.
#'
#' @param ipa A named list following the format `list(representation = character(), articulation = character())` with each element representing
#' the vowel's IPA representation (e.g. "a", "p", etc.) and phonetic dimensions respectively.
#'
#' @return A vowel object.
#'
#' @examples
#' ipa_data <- list(representation = "e", articulation = c("close-mid", "front", "unrounded"))
#' e <- vowel(ipa_data)
#'
#' @export
#'
vowel <- function(ipa) {
  validate_phoneme(new_vowel(ipa))
}
