% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg-100doors.R
\name{play_game}
\alias{play_game}
\title{Game play function to run through the game twice, once switching envelopes and
once keeping the original envelope}
\usage{
play_game()
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
Data frame with results of keeping envelope and switching
}
\description{
\code{play_game()} pulls in all elements of the game to determine if switching or keeping the
envelope results in a win
}
\details{
The function imports monty_hall_100doors() and extracts the outcome variable
from the resulting list to indicate if there is a win or loss
}
\examples{
play_game()
}
