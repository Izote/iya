% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phoneme.R
\name{consonant}
\alias{consonant}
\title{Create a new consonant object.}
\usage{
consonant(ipa, description)
}
\arguments{
\item{ipa}{The IPA representation of the consonant as a length 1 character
vector. Unicode escape sequences can be utilized for various diacritic marks.}

\item{description}{A description of this consonant's linguistic qualities
as a character vector. Each discrete descriptive quality included must map
to a single element of the vector.

For example, \code{c("voiced", "bilabial", "nasal")} would be a valid vector}
}
\value{
A consonant object.
}
\description{
Takes IPA data as a list of character vectors and returns a consonant object.
A consonant object is a subclass of the more general phoneme object.
}
\examples{
p <- consonant("p", c("voiceless", "bilabial", "plosive"))

}
