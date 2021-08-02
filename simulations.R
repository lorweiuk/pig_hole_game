
################################
# simulating the pig hole game #
################################

source("C:/Users/User/Documents/pig_hole_game/pig_hole_game_function.R")

keep <- list(pig_hole_game = pig_hole_game)

rm(pig_hole_game)

# define matrix plotting function
keep$mat_plot <- function(mat, ...) {
  # plot each line in matrix as line type "b", with colors from dark to light grey
  for (r in 1:nrow(mat)) {
    
    plot(mat[r, ], type = "b", col = rgb(r / nrow(mat), r / nrow(mat), r / nrow(mat)), ...)
    
    if (r < nrow(mat))
      par(new=T)
  }
}

####################################################################################
# when both players follow simple fill-N-then-yield strategy, which N is the best? #
####################################################################################

keep$which_N <- list() # list to store results of simulations

keep$which_N$n_sim <- 300 # number of simulated games per setting

keep$which_N$player_succ               <- matrix(NaN, nrow = 5, ncol = 5) # proportion games the player wins
keep$which_N$which_N_num_rounds        <- matrix(NaN, nrow = 5, ncol = 5) # mean number of rounds for game
keep$which_N$which_N_num_rounds_excess <- matrix(NaN, nrow = 5, ncol = 5) # number of games in which max number of rounds was exceeded

for (plf in 1:5) {
  
  # function defining player's strategy: fill N fields, then yield
  # N is given by variable plf
  strategy_player <- function(board, own_marbles, other_marbles) {
    if (sum(board[1:5] == c(1, 2, 3, 4, 5)) >= plf) {
      return(1)
    } else {
      return(0)
    }
  }
  
  for (enf in 1:5) {
    
    # print out status
    current_prog <- ((plf - 1) * 5 + enf)*100 / 25
    
    print(paste("Current progress:", current_prog, "%"))
    
    # opponent's strategy function, analogous to strategy_player()
    strategy_opponent <- function(board, own_marbles, other_marbles) {
      if (sum(board[1:5] == c(1, 2, 3, 4, 5)) >= enf) {
        return(1)
      } else {
        return(0)
      }
    }
    
    num_succ <- rep(0, times = keep$which_N$n_sim)
    n_rounds <- rep(0, times = keep$which_N$n_sim)
    n_excess <- rep(0, times = keep$which_N$n_sim)
    
    for (i in 1:keep$which_N$n_sim) {
      # simulate game
      temp <- keep$pig_hole_game(400, 30, 30, sample(c(T, F), size = 1), TRUE, strategy_player, strategy_opponent)
      
      # round in which game ended
      last_nonan <- max(which(!is.na(temp[, 9])))
      
      # did player win?
      if (temp[last_nonan, 9] < temp[last_nonan, 10])
        num_succ[i] = 1
      
      n_rounds[i] = n_rounds[i] + last_nonan
      
      if (last_nonan == 300)
        n_excess[i] = 1
    }
    
    keep$which_N$player_succ[plf, enf] = mean(num_succ)
    keep$which_N$which_N_num_rounds[plf, enf] = mean(n_rounds)
    keep$which_N$which_N_num_rounds_excess[plf, enf] = mean(n_excess)
  }
}

# remove everything but results and game function
rm(list = ls()[ls() != "keep"])

# visualize
windows()
keep$mat_plot(keep$which_N$player_succ, ylim = c(0, 1), ylab = "proportion player success", xlab = "which N opponent")

###################################################
# which N is best if opponent has 20-marble lead? #
###################################################

keep$lead_20 <- list()

keep$lead_20$n_sim <- 300

keep$lead_20$player_succ       <- matrix(NaN, nrow = 5, ncol = 5) # proportion games the player wins
keep$lead_20$num_rounds        <- matrix(NaN, nrow = 5, ncol = 5) # mean number of rounds for game
keep$lead_20$num_rounds_excess <- matrix(NaN, nrow = 5, ncol = 5) # number of games in which max number of rounds was exceeded

for (plf in 1:5) {
  
  # function defining player's strategy: fill N fields, then yield
  # N is given by variable plf
  strategy_player <- function(board, own_marbles, other_marbles) {
    if (sum(board[1:5] == c(1, 2, 3, 4, 5)) >= plf) {
      return(1)
    } else {
      return(0)
    }
  }
  
  for (enf in 1:5) {
    
    # print out status
    current_prog <- ((plf - 1) * 5 + enf)*100 / 25
    
    print(paste("Current progress:", current_prog, "%"))
    
    # opponent's strategy function, analogous to strategy_player()
    strategy_opponent <- function(board, own_marbles, other_marbles) {
      if (sum(board[1:5] == c(1, 2, 3, 4, 5)) >= enf) {
        return(1)
      } else {
        return(0)
      }
    }
    
    num_succ <- rep(0, times = keep$lead_20$n_sim)
    n_rounds <- rep(0, times = keep$lead_20$n_sim)
    n_excess <- rep(0, times = keep$lead_20$n_sim)
    
    for (i in 1:keep$lead_15$n_sim) {
      # simulate game
      temp <- keep$pig_hole_game(400, 30, 10, sample(c(T, F), size = 1), TRUE, strategy_player, strategy_opponent)
      
      # round in which game ended
      last_nonan <- max(which(!is.na(temp[, 9])))
      
      # did player win?
      if (temp[last_nonan, 9] < temp[last_nonan, 10])
        num_succ[i] = 1
      
      n_rounds[i] = n_rounds[i] + last_nonan
      
      if (last_nonan == 300)
        n_excess[i] = 1
    }
    
    keep$lead_20$player_succ[plf, enf] = mean(num_succ)
    keep$lead_20$num_rounds[plf, enf] = mean(n_rounds)
    keep$lead_20$num_rounds_excess[plf, enf] = mean(n_excess)
  }
}

# remove everything but results and game function
rm(list = ls()[ls() != "keep"])

# visualize
windows()
keep$mat_plot(keep$lead_20$player_succ, ylim = c(0, 1), ylab = "proportion player success", xlab = "which N opponent")

# measure n-rounds-until-full and n-fails for 1 to 5 fields full
n_sims <- 50000

n_till_full <- matrix(NaN, nrow = n_sims, ncol = 5)

for (i in 1:n_sims) {
  # print progress
  if (i %% 100)
    print(i * 100 / n_sims)
  
  temp <- pig_hole_game(100, 30, TRUE, FALSE, function(x,y,z) { return(0) }, function(x,y,z) { return(1) })
  
  full <- temp[, 3:7]
  
  first_fail <- min(which(temp[,2] == 2)) - 1
  
  full <- full[1:(first_fail - 1), ]
  
  if (!is.null(dim(full))) {
    full <- full == matrix(1:5, nrow = nrow(full), ncol = ncol(full), byrow = T)
    full <- rowSums(matrix(as.integer(full), ncol=5)) # compute number of round until full, for 1 to 5
    full <- sapply(1:5, function(x) { min(which(full==x)) })
    full[full==Inf] = NaN
  } else {
    full <- full == c(1, 2, 3, 4, 5)
    temp <- rep(NaN, 5)
    temp[sum(as.integer(full))] = 1
    full <- temp
  }
  
  n_till_full[i, ] = full
}

colMeans(n_till_full, na.rm = T)
colSums(is.na(n_till_full)) / n_sims





