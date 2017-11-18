#----------------1 function-----------------------#
sum_column <- function(d, var) {
  result <- NULL
  x <- d[[var]] 
  if (!is.null(x)) { 
    if(is.numeric(x)){
      result<-sum(x)
    }
  }
  return (result)
}

#----------------2 function-----------------------#
my_sum<-function(vector){
  sum <-0                     #at first the summary is 0
  for(i in 1:length(vector)){ #this loop is to take every value out one by one
    sum<-sum+vector[i]        #add these values one by one
  }
 return(sum)
}

#----------------3 function-----------------------#
sum_divided_by<-function(vector,number){
  somme<-0
  #here the 'if' sentence to verify if the two arguments are numeric,
  #if one of them is not---> return the NULL
  #if they are both numeric--->add the vector get the summary and divide the summary by 'number'
  if (!is.numeric(vector)||!is.numeric(number)){
    return (NULL)
  }
  else{
    for(i in 1:length(vector)){ 
      somme<-somme+vector[i]
    }
    return (somme/number)
  }
}

#----------------4 function-----------------------#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))+ ggplot2::geom_violin() 
  # YOUR CODE HERE: Create a violin plot
  return(p)
}

#----------------5 function-----------------------#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))+ ggplot2::geom_violin() }

#----------------6 function-----------------------#
my_mean<-function(vector){
  if (!is.numeric(vector)){#verify the argument type
    return (NULL)
  }
  else{
    len<-length(vector)#we want to get the sum divided by the number of elements
                       #so we need the number of elements,which is just the length of vector
    return(sum_divided_by(vector,len))
  }
}

#----------------7 function-----------------------#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result<-median(d_1[,c(var)])-median(d_2[,c(var)])
  
  return(result)
}

#----------------8 function-----------------------#
randomize <- function(d, var) {
  n<-nrow(d)
  d[[var]] <- sample(d[,c(var)],n)
  return(d)}

#----------------9 function-----------------------#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    # fill in the vector permutation_statistics with the
    # value of statistic(...) for this new permutation
    d_fake<-randomize(d,var)
    d_1_fake<-dplyr::filter(d_fake, get(grouping_var)  == group1)
    d_2_fake<-dplyr::filter(d_fake, get(grouping_var)  == group2)
    permutation_statistics[i] <- median(d_1_fake[,c(var)]) - median(d_2_fake[,c(var)])
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

#---------------10function----------------------------#
new_test_statistic<-function(d, var, grouping_var, group1, group2){
  result<-0.1
  return(result)
}

#---------------11function----------------------------#
#分别计算vline左边和右边分布的数量的百分比
permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}
permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}
