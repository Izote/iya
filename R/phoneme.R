#' Constructor for the phoneme class.
#'
#' @param x A list containing IPA data.
#' @param class A single-element character vector - "consonant" or "vowel".
#'
#' @return A phoneme class object.
#'
new_phoneme <- function(x, class = character()) {
  structure(x, class = c(class, "phoneme"))
}


#' Constructor for the consonant (sub)class.
#'
#' @param x A list containing IPA data.
#'
#' @return A consonant (sub)class object.
#'
new_consonant <- function(x) {
  new_phoneme(x, class = "consonant")
}


#' Constructor for the vowel (sub)class.
#'
#' @param x A list containing IPA data.
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
    stop("Constructor could not bind provided arguments to a list.")
  }

  if (length(x) < 2) {
    stop("Constructor takes 2 arguments. Insufficient arguments provided.")
  }

  if (length(x) > 2) {
    stop("Constructor takes 2 arguments. Too many arguments provided.")
  }

  if (!all(sapply(x, is.character))) {
    stop("Constructor only takes character vectors.")
  }

  x
}


#' Create a new consonant object.
#'
#' Takes IPA data as a list of character vectors and returns a consonant object.
#' A consonant object is a subclass of the more general phoneme object.
#'
#' @param ipa The IPA representation of the consonant as a length 1 character
#' vector. Unicode escape sequences can be utilized for various diacritic marks.
#'
#' @param description A description of this consonant's linguistic qualities
#' as a character vector. Each discrete descriptive quality included must map
#' to a single element of the vector.
#'
#' For example, `c("voiced", "bilabial", "nasal")` would be a valid vector
#'
#' @return A consonant object.
#'
#' @examples
#' p <- consonant("p", c("voiceless", "bilabial", "plosive"))
#'
#' @export
#'
consonant <- function(ipa, description) {
  validate_phoneme(new_consonant(list(ipa = ipa, description = description)))
}


#' Create a new vowel object.
#'
#' Takes IPA data as a list of character vectors and returns a vowel object.
#' A vowel object is a subclass of the more general phoneme object.
#'
#' @param ipa The IPA representation of the vowel as a length 1 character
#' vector. Unicode escape sequences can be utilized for various diacritic marks.
#'
#' @param description A description of this vowel's linguistic qualities
#' as a character vector. Each discrete descriptive quality included must map
#' to a single element of the vector.
#'
#' For example, `c("close", "front", "unrounded")` would be a valid vector
#'
#' @return A vowel object.
#'
#' @examples
#' a <- vowel("a", c("open", "central", "unrounded"))
#'
#' @export
#'
vowel <- function(ipa, description) {
  validate_phoneme(new_vowel(list(ipa = ipa, description = description)))
}
