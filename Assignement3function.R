
  difference_in_proportion <- function(d, var, grouping_var, group1, group2,
                                       target_value) {
    the_group_1<-dplyr::filter(d,get(grouping_var)==group1)
    the_group_2<-dplyr::filter(d,get(grouping_var)==group2)
    
    group_1_nb_col<-nrow(dplyr::filter(the_group_1,get(var)==target_value))
    group_2_nb_col<-nrow(dplyr::filter(the_group_2,get(var)==target_value))
    
    result<-group_1_nb_col/nrow(the_group_1)-group_2_nb_col/nrow(the_group_2)
    return (result)
  }
  
 #---------------function 2----------------------# 
  randomize <- function(d, var) {
    d[,var]<-sample(d[[var]], nrow(d))
    return(d)}
  
  
  permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                    n_samples=9999,target_value) {
    observed_statistic <- statistic(d, var, grouping_var, group1, group2,target_value)
    permutation_statistics <- rep(0, n_samples)
    for (i in 1:n_samples) {
      # YOUR CODE HERE: use randomize(...) to create a permutation and then
      # fill in the vector permutation_statistics with the
      # value of statistic(...) for this new permutation
      d_fake<-randomize(d,var)
      
      permutation_statistics[i] <- statistic(d_fake,var, grouping_var, group1, group2,target_value)
    }
    result <- list(observed=observed_statistic,
                   permuted=permutation_statistics)
    return(result)
  }
  
  
  #---------------function 3---------------------#
  permtest_difference_in_props <- function(k1, k2, n1, n2, n_samples=99) {
    obs_1 <- c(rep(TRUE, k1), rep(FALSE, n1 - k1)) # Individual observations from group 1
    obs_2 <- c(rep(TRUE, k2), rep(FALSE, n2 - k2)) # Individual observations from group 2
    observations <- c(obs_1, obs_2)
    rep_observations <- matrix(rep(observations, n_samples), n1 + n2)
    perm_observations <- apply(rep_observations, 2, sample, n1 + n2)
    props_group1 <- colMeans(perm_observations[1:n1,])
    props_group2 <- colMeans(perm_observations[(n1+1):(n1+n2),])
    test_stats <- props_group1 - props_group2
    return(list(observed=((k1/n1) - (k2/n2)), permuted=test_stats))
  }
  
  #-------------function 4------------------------#
  v_pdp_pvalue_right <- function(k1_vec, k2_vec, n1, n2, n_samples=99) {
    library(foreach)
    library(doParallel)
    cores=detectCores()
    cl<-makeCluster(cores[1]-1)
    
    registerDoParallel(cl)
    
    trials<-length(k1_vec)
    result <- rep(NA, length(k1_vec))
    foreach(i=1:trials) %dopar%  {
      result[i]<-permtest_difference_in_props(k1_vec[i],k2_vec[i],n1,n2)
    }
    return(result)
  }
  
  
  #-------------funcion 5----------------------#
  pearson_x2_stat <- function(d, var, grouping_var) {
    values <- unique(d[[var]])
    groups <- unique(d[[grouping_var]])
    result <- 0
    for (v in values) {
      prop_overall <- mean(d[[var]] == v)
      for (g in groups) {
        d_g <- dplyr::filter(d, get(grouping_var) == g)
        prop_g <- mean(d_g[[var]] == v)
        result <- result + ((prop_g - prop_overall)^2)*(nrow(d_g)/prop_overall)
      }
    }
    return(result)
  }
  
#-------------function 6-------------------------#
  permutation_test <- function(d, var_to_permute, statistic, n_samples=9999, group_var) {
    observed_statistic <- statistic(d,var_to_permute, group_var)
    permutation_statistics <- rep(0, n_samples)
    for (i in 1:n_samples) {
      d_perm <- randomize(d, var_to_permute)
      permutation_statistics[i] <- statistic(d_perm, group_var)
    }
    result <- list(observed=observed_statistic,
                   permuted=permutation_statistics)
    return(result)
  }
   