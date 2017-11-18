
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
   