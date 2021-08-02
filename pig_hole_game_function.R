
pig_hole_game <- function(n_max_rounds, n_marbles_each, player_begin, include_warmup, strategy_player, strategy_enemy) {
    
  # dice throws for all rounds
  rnd_dice <- round(runif(n = n_max_rounds, min = 0.5000001, max = 6.4999999))
  
  # simulate all rounds
  board_state <- matrix(0, nrow = n_max_rounds, ncol = 6)
  
  # marble count for player (col 1) and opponent (col 2)
  n_marbles <- matrix(NaN, nrow = n_max_rounds, ncol = 2)
  n_marbles[1, ] = n_marbles_each
  
  # record of who's turn it is
  players_turn <- rep(NaN, times = n_max_rounds)
  
  players_round <- if (player_begin) { 1 } else { 2 } # who's turn is it? column indicator for n_marbles matrix
  
  for (nr in 1:n_max_rounds) {
    
    # register who's turn it is
    players_turn[nr] = players_round
    
    # register dice roll
    if (nr > 1)
      board_state[nr, ] = board_state[nr - 1, ] # copy previous round's state

    board_state[nr, rnd_dice[nr]] = board_state[nr, rnd_dice[nr]] + 1 # add marble according to dice
    
    # copy previous round's scores
    if (nr > 1)
      n_marbles[nr, ] = n_marbles[nr - 1, ]
    
    n_marbles[nr, players_round] = n_marbles[nr, players_round] - 1 # remove marble from player
    
    # check if too many marbles in field
    if (any(board_state[nr, 1:5] > 1:5)) {
      # add content of board to current player's marble count; subtract one for the currently moved stone
      n_marbles[nr, players_round] = n_marbles[nr, players_round] + sum(board_state[nr, 1:5])
      
      board_state[nr, 1:5] = 0 # reset board
      
      players_round = 3 - players_round # change player
      
    } else if (include_warmup && nr <= 6) {
      # if game is still in the first 6 rounds ... don't allow strategy
      
      if (nr == 1 || nr == 2 || nr == 4 || nr == 6) {
        # on rounds 1, 2, 4 and 6, force player change
        players_round = 3 - players_round
      }
      
    } else {
      # after 6 rounds, and if no field is overfilled: allow strategy
      decision <- NULL
      
      if (players_round == 1) {
        decision <- strategy_player(board_state[nr, ], n_marbles[nr, 1], n_marbles[nr, 2])
        
      } else if (players_round == 2) {
        decision <- strategy_enemy(board_state[nr, ], n_marbles[nr, 2], n_marbles[nr, 1])
      }
      
      if (decision)
        players_round = 3 - players_round # change player
    }
    
    # check if game is over
    if (any(n_marbles[nr, ] == 0))
      break
  }
  
  # return results
  output <- cbind(rnd_dice, players_turn, board_state, n_marbles)
  
  colnames(output) = c("dice", "turn", "board_1", "board_2", "board_3", "board_4", "board_5", "board_6", "marbles_player", "marbles_enemy")
  
  return(output)
}

############
# simulate #
############

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





