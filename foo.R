# File Content:
#
# Several R functions, with master functions simulate_games() and play_game()
# which allow us to recreate linear contextual bandit algorithms.
# One can modulate context generation via generate_set_xta(), which can be rendered
# non-stationary and one can also propose new policies via generate_at().

#' @title Generate context vectors (x_{ta} ; a \in [K]) based on memory game and parameters
generate_set_xta <- function(game_config, game_memory, time){

  # extract context generation scenario  
  context_gen <- game_config$CONTEXT_GEN_VALUE 

  ##  
  if( context_gen == CONTEXT_GEN_VALUE[1] ){
    
    # generate identity matrix for the MAB setting
    xt <- diag( game_config$d )
    
  }else if( context_gen == CONTEXT_GEN_VALUE[2] ){
    
    # generate N(0,1) entities for each context value, truncated at 4 
    xt <- rnorm(n = game_config$K * game_config$d, mean = 0, sd = 1)
    xt[xt > +4] <- 4
    xt[xt < -4] <- (-4)
    xt <- matrix(data = xt, nrow = game_config$K, ncol = game_config$d, byrow = TRUE)
    
  }else{
    
    stop('Context generation method not supported yet.')
    
  }
  
}

#' @title Generate action a_t
generate_at <- function(game_config, game_memory, time){
  
  ##
  if( game_config$strategy == 'greedy' ){
    
    # compute \forall a \in [K], x_{ta} \hat{\beta} if \hat{\beta} defined
    if( game_memory$estimator$exploit == FALSE ){
      bh <- game_memory$estimator$beta_0
    }else{
      bh <- game_memory$estimator$beta_hat
    }
    expected_rt_hat <- game_memory$set_xta %*% bh
    at <- which(expected_rt_hat == max(expected_rt_hat))
    
  }else if( game_config$strategy == 'ucb' ){
    
    if( time == 1 ){
      at <- 1:(game_config$K)
    }else{
      # compute \forall a \in [K], x_{ta} \hat{\beta} + alpha || x_{ta} ||_{M_t^{-1}} 
      expected_rt_hat <- game_memory$set_xta %*% game_memory$estimator$beta_hat_ridge
      norm_xta_wrt_xtx_inv <- sapply(X = 1:game_config$K, FUN = function(j){
        game_memory$set_xta[j,] %*% game_memory$estimator$xtx_inv_ridge %*% game_memory$set_xta[j,]
      })
      ct <- expected_rt_hat + game_config$alpha * sqrt(norm_xta_wrt_xtx_inv)
      at <- which(ct == max(ct))
    }
    
  }else{
    stop('Strategy not supported.')
  }
  
  # if ties, select at random
  if(length(at) > 1) at <- sample(x = at, size = 1)
  
  return( at )
  
}

#' @title Generate reward r_t
generate_rt <- function(game_config, game_memory, time){
  expected_rt <- game_memory$set_xta[game_memory$history$at[time],] %*% game_config$beta
  rt <- expected_rt + rnorm(n = 1, mean = 0, sd = sqrt(game_config$set_s2[game_memory$history$at[time]]))
  rt <- as.numeric(rt)
  return(rt)
}

#' @title Generate instantaneous regret
generate_irt <- function(game_config, game_memory, time){
  expected_rt <- game_memory$set_xta %*% game_config$beta
  irt <- max(expected_rt) - expected_rt[game_memory$history$at[time]]
  return(irt)
}

#' @title Compute beta hat
compute_beta_hat <- function(game_config, game_memory, time){
  # check invertibility of xtx
  if( game_memory$estimator$exploit == FALSE & eigen(game_memory$estimator$xtx)$values[game_config$d] > 1e-10 ){
    game_memory$estimator$exploit <- TRUE
  }
  # ridge estimator (lambda = 1)
  game_memory$estimator$xtx_inv_ridge <- solve(diag(game_config$d) + game_memory$estimator$xtx)
  game_memory$estimator$beta_hat_ridge <- (game_memory$estimator$xtx_inv_ridge %*% game_memory$estimator$xtr)
  # ols estimator
  if( game_memory$estimator$exploit == TRUE ){
    game_memory$estimator$xtx_inv <- solve(game_memory$estimator$xtx)
    game_memory$estimator$beta_hat <- (game_memory$estimator$xtx_inv %*% game_memory$estimator$xtr)
  }
  return(game_memory)
}

#' @title Initiate game memory dataframe
init_game_memory <- function(game_config){
  
  # constants
  K <- game_config$K
  d <- game_config$d
  n <- game_config$game_duration
  
  # game record
  xt <- array(data = NA, dim = c(n, d))
  gm1 <- data.frame('t' = 1:n, 'at' = NA, 'xt' = xt, 'rt' = NA, 'irt' = NA)
  
  # estimator
  dbyd_mat <- array(data = 0, dim = c(d,d))
  gm2 <- list('exploit' = FALSE,
              'beta_0' = rep(0, d),
              'xtx' = array(data = 0, dim = c(d,d)), 
              'xtr' = array(data = 0, dim = d))
  
  # set xta recently generated
  gm3 <- array(data = NA, dim = c(K,d))
  
  # compil
  gm <- list('history' = gm1, 'estimator' = gm2, 'set_xta' = gm3)
  return(gm)
}

#' @title Play game
play_game <- function(game_config){
  
  # get game configuration
  gc <- game_config
  # get game duration
  n <- gc$game_duration
  # initiate game memory
  gm <- init_game_memory(game_config = game_config)
  # set seed for reproducibility
  set.seed(gc$seed)
  # get column indexes for the context vector
  index_col_x <- which(substr(colnames(gm$history), 1, 2) == 'xt')
  
  # generate data
  for(i in 1:n){
    
    # generate contexts
    set_xta <- generate_set_xta(game_config = gc, game_memory = gm, time = i)
    stopifnot(all(is.finite(set_xta) & !is.na(set_xta)))
    gm$set_xta <- set_xta
    
    # take decision
    at <- generate_at(game_config = gc, game_memory = gm, time = i)
    stopifnot(at %in% 1:gc$K)
    gm$history$at[i] <- at
    
    # collect reward
    rt <- generate_rt(game_config = gc, game_memory = gm, time = i)
    gm$history$rt[i] <- rt
    
    # record instantaneous regret
    irt <- generate_irt(game_config = gc, game_memory = gm, time = i)
    gm$history$irt[i] <- irt
    
    # fill in associated context
    sel_xta <- set_xta[at,]
    gm$history[i, index_col_x] <- set_xta[at,]
    gm$estimator$xtx <- gm$estimator$xtx + sel_xta %*% t(sel_xta)
    gm$estimator$xtr <- gm$estimator$xtr + sel_xta * rt
    
    # udpate estimates
    gm <- compute_beta_hat(game_config = gc, game_memory = gm, time = i)
    
  }
  
  # return dataset
  return(gm)
  
}

#' @title Simulate several games and output cumulative regret
#' @param n_games Number of simulations to run
#' @param game_config Generic game configuration + generic seed element required
simulate_games <- function(n_games, game_config){
  
  # number of iter
  nbe <- n_games
  n <- game_config$game_duration
  
  #  generate seeds
  set.seed(game_config$generic_seed)
  seeds <- sample(x = 1000:9999, size = nbe)
  
  # iterate over experiments
  regret_res <- data.frame(
    "id" = rep(0, times = n*nbe*2),
    "time" = rep(0, times = n*nbe*2),
    "strategy" = "", # factor(rep(c('greedy', 'ucb', 'ucb_n'), each = nbe)),
    "regret" = -1, #c(c(regret_mem[, 1:100]), c(regret_mem[, 100 + 1:100]), c(regret_mem[, 200 + 1:100]))
    stringsAsFactors = FALSE
  )
  
  #initiate experiment count
  e_count <- 0
  for(e in 1:2){
    
    # choose strategy
    if(e == 1){
      game_config$strategy <- 'greedy'  
    }else if(e == 2){
      game_config$strategy <- 'ucb'
    }
    
    # do set of experiments
    for(iter in 1:nbe){
      # set seed and play game    
      game_config$seed <- seeds[iter]
      g <- play_game(game_config)
      # store regret
      e_count <- e_count + 1
      #
      indexes <- 1:n + n * (e_count-1)
      regret_res[indexes, "id"] <- iter
      regret_res[indexes, "time"] <- 1:n
      regret_res[indexes, "strategy"] <- game_config$strategy
      regret_res[indexes, "regret"] <- cumsum(g$history$irt)
    }
    
  }
  
  return(regret_res)
  
}
