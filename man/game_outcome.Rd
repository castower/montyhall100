% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg-100doors.R
\name{game_outcome}
\alias{game_outcome}
\title{Identify if contestant wins or loses the game}
\usage{
game_outcome(final_choice)
}
\arguments{
\item{final_choice}{Numeric integer}
}
\value{
Character string, "WIN" or "LOSE"
}
\description{
\code{game_outcome()} identifies whether the final choice from final_contestant_choice()
results in the contestant winning
}
\details{
The function checks if the value of final choice is $1000
}
\examples{
game_outcome("newsprint")
game_outcome("$1000")
}
