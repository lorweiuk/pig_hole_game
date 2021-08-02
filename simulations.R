
################################
# simulating the pig hole game #
################################

n_sim <- 300

player_succ <- matrix(NaN, nrow = 5, ncol = 5)
num_rounds  <- matrix(NaN, nrow = 5, ncol = 5)

for (plf in 1:5) {
  
  strategy_player <- function(board, own_marbles, opponent_marbles) {
    # function defining player's strategy
    if (sum(board[1:5] == c(1, 2, 3, 4, 5)) >= plf) {
      return(1)
    } else {
      return(0)
    }
  }
  
  for (enf in 1:5) {
    
    strategy_enemy <- function(board, own_marbles, opponent_marbles) {
      # function defining player's strategy
      if (sum(board[1:5] == c(1, 2, 3, 4, 5)) >= enf) {
        return(1)
      } else {
        return(0)
      }
    }
    
    num_succ <- rep(0, times = n_sim)
    n_rounds <- rep(0, times = n_sim)
    
    for (i in 1:n_sim) {
      temp <- pig_hole_game(300, 30, sample(c(T, F), size = 1), TRUE, strategy_player, strategy_enemy)
      
      last_nonan <- max(which(!is.na(temp[, 9])))
      
      if (temp[last_nonan, 9] < temp[last_nonan, 10])
        num_succ[i] = 1
      
      n_rounds[i] = n_rounds[i] + last_nonan
    }
    
    player_succ[plf, enf] = mean(num_succ)
    
    num_rounds[plf, enf] = mean(n_rounds)
  }
}

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





