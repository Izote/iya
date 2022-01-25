#' Constructor for the phoneme class.
#'
#' @param x A named list containing IPA data.
#' @param class A single-element character vector - "consonant" or "vowel".
#'
#' @return A phoneme class object.
#'
new_phoneme <- function(x, class = character()) {
  structure(x, class = c(class, "phoneme"))
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

  if (length(x) != 2) {
    stop("Provided list must be of length 2.")
  }

  if (!all(sapply(x, is.character))) {
    stop("Provided list must only contain character vectors.")
  }

  if (length(x[[2]]) != 3) {
    stop("Second element of list must be a vector of length 3.")
  }

  x
}


#' Create a new consonant object.
#'
#' Takes IPA data as a list of character vectors and returns a consonant object.
#' A consonant object is a subclass of the more general phoneme object.
#'
#' @param ipa A list of two character vectors. The first element provides
#' the IPA representation for the desired phoneme and the second provides its
#' articulation as a character vector of length 3.
#'
#'
#' @return A consonant object.
#'
#' @examples
#' ipa_data <- list("p", c("voiceless", "bilabial", "plosive"))
#' p <- consonant(ipa_data)
#'
#' @export
#'
consonant <- function(ipa) {
  validate_phoneme(new_consonant(ipa))
}


#' Create a new vowel object.
#'
#' Takes IPA data as a list of character vectors and returns a vowel object.
#' A vowel object is a subclass of the more general phoneme object.
#'
#' @param ipa A list of two character vectors. The first element provides
#' the IPA representation for the desired phoneme and the second provides its
#' articulation as a character vector of length 3.
#'
#'
#' @return A consonant object.
#'
#' @examples
#' ipa_data <- list("e", c("close-mid", "front", "unrounded"))
#' e <- vowel(ipa_data)
#'
#' @export
#'
vowel <- function(ipa) {
  validate_phoneme(new_vowel(ipa))
}
