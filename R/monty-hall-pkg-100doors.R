#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `start_game()` generates a new game that consists of 100 envelopes 
#'   with 99 filled with newsprint and one with a $1000 dollars.
#'
#' @details
#'   The game setup replicates an alternate version of the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   start_game()
#'
#' @export
start_game <- function() {
  
  # Create list of envelope stuffers
  envelope_stuffers <- c("newsprint", "$1000")
  for (i in 1:98) {
    #print(i)
    envelope_stuffers <- append(envelope_stuffers, "newsprint")
  }
  
  # Place them randomly in game envelopes
  game_envelopes <- sample(envelope_stuffers, size=100, replace = F)
  
  return(game_envelopes)
}



#' @title
#' Simulate contestant's selection of first envelope
#' @description
#' `select_contestant_choice()` utilizes 'sample' to select a number between 1-100 that 
#' corresponds to the number of envelopes
#' @details
#' This function is the first step after starting the game and represents a 1/100 chance of 
#' selecting the $1000 envelope.
#' @param ... no arguments are used by the function.
#' @return The function returns a integer between 1 and 100
#' @examples
#' select_contestant_choice()
#' @export
select_contestant_choice <- function() {
  choice <- sample(1:100, size=1, replace = F)
  return(choice)
}



#' @title
#' Simulate host's selection of 98 envelopes containing newsprint
#' @description
#' `select_host_choice()` utilizes the R functions which and sample to randomly 
#' select 98 envelopes that have newsprint in them.
#' @details
#' This function is the second step in the game and eliminates almost all of the remaining 
#' envelopes available for the contestant. Also guarantees that only newsprint envelopes
#' are eliminated.
#' @param envelopes Character vector
#' @param contestant_choice Numeric integer
#' @return The function returns a numeric vector
#' @examples
#' select_host_choice(1:100, 13)
#' @export
select_host_choice <- function(envelopes, contestant_choice) {
  
  # Update envelope options by eliminating contestant's choice
  host_envelopes <- envelopes[-contestant_choice]
  
  host_choice <- sample(which(host_envelopes == "newsprint"), size=98, replace = F)
  
  host_choice <- c(host_choice)
  
  return(host_choice)
  
}


#' @title
#' Determine whether contestant decides to take the final envelope or keeps their original 
#' choice.
#' @description
#' `final_contestant_choice()` filters out all envelope options to remove the contestant 
#' and host's combined 99 selections and leaves one envelope. Also allows contestant
#' to decide whether to keep their original envelope or switch.
#' @details
#' This function is the third step in the game and ultimately determines whether the 
#' contestant wins or loses
#' @param envelopes Character vector
#' @param contestant_choice Numeric integer
#' @param host_choice Numeric vector, 98 numbers
#' @param switch_choice Character, string to determine whether to SWITCH or KEEP
#' @return The function returns a character string of newsprint or $1000
#' @examples
#' final_contestant_choice(1:100, 13, 1:98, "SWITCH")
#' @export
final_contestant_choice <- function(envelopes, contestant_choice, host_choice, switch_choice) {
  
  final_envelope <- envelopes[-contestant_choice]
  final_envelope <- final_envelope[-host_choice]
  
  #print(switch_choice)
  
  if (toupper(switch_choice) == "SWITCH") {
    out <- final_envelope
  } else {
    out <- envelopes[contestant_choice]
  }
  
  return(out)
}



#' @title
#' Identify if contestant wins or loses the game
#' @description
#' `game_outcome()` identifies whether the final choice from final_contestant_choice()
#' results in the contestant winning
#' @details
#' The function checks if the value of final choice is $1000
#' @param final_choice Numeric integer
#' @return Character string, "WIN" or "LOSE"
#' @examples
#' game_outcome("newsprint")
#' game_outcome("$1000")
#' @export
game_outcome <- function(final_choice) {
  if (final_choice == "$1000") {
    return("WIN")
  } else {
    return("LOSE")
  }
}




#' @title
#' Run through game and store outcome values
#' @description
#' `monty_hall_100doors()` plays the entire game and returns a list of game outcomes 
#' @details
#' The function provides an output of all contents of the 100 envelopes, the contestant's
#' original choice, the host's choice of 98 envelopes, the contestant's decision to switch,
#' the final choice, and the final game outcome
#' @param switch_choice Character string, "KEEP" or "SWITCH"
#' @return List with labels for stored values
#' @examples
#' monty_hall_100doors("SWITCH")
#' monty_hall_100doors("KEEP")
#' @export
monty_hall_100doors <- function(switch_choice) {
  
  envelopes <- start_game()
  contestant_choice <- select_contestant_choice()
  host_choice <- select_host_choice(envelopes, contestant_choice)
  final_choice <- final_contestant_choice(envelopes, contestant_choice, host_choice, switch_choice)
  outcome <- game_outcome(final_choice)
  
  return(list(envelopes = envelopes, 
              contestant_choice = contestant_choice, 
              host_choice = host_choice, 
              switch_choice = switch_choice, 
              final_choice = final_choice, 
              outcome = outcome))
}



#' @title
#' Game play function to run through the game twice, once switching envelopes and 
#' once keeping the original envelope
#' @description
#' `play_game()` pulls in all elements of the game to determine if switching or keeping the 
#' envelope results in a win
#' @details
#' The function imports monty_hall_100doors() and extracts the outcome variable 
#' from the resulting list to indicate if there is a win or loss
#' @param ... no arguments are used by the function.
#' @return Data frame with results of keeping envelope and switching
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  outcome.stay <- monty_hall_100doors("KEEP")$outcome
  outcome.switch <- monty_hall_100doors("SWITCH")$outcome
  
  # game.results <- bundle the results
  # return( <<< game.results >>> )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}




#' @title
#' Game simulation function
#' @description
#' `play_n_games()` performs a Monte Carlo simulation to play through the game
#' switching and keeping the envelope multiple times and produces an output table
#' to indicate the percentage of wins and losses for each strategy
#' @details
#' The function loops through play_game() multiple times and returns data frames of outcomes
#' and proportion of wins and losses for each strategy
#' @param n Integer for number of simulations
#' @return Proportion table and data frame of outcomes
#' @examples
#' play_n_games(10)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
