source("R/functions.R")

generate_external_swap <- function(date_d)
{
  d_int <- as.integer(as.character(date_d, format="%d"))
  m_int <- as.integer(as.character(date_d, format="%m"))
  Y_int <- as.integer(as.character(date_d, format="%Y"))
  
  es_date <- as.Date(d_m_Y_to_date_string(day=m_int,month=d_int,year=Y_int), "%d/%m/%Y")
  
  return(es_date)
}

generate_internal_swap <- function(date_d)
{
  d_int <- as.integer(as.character(date_d, format="%d"))
  m_int <- as.integer(as.character(date_d, format="%m"))
  Y_int <- as.integer(as.character(date_d, format="%Y"))
  
  d10 <- d_int %/% 10
  d1 <- d_int %% 10
  m10 <- m_int %/% 10
  m1 <- m_int %% 10
  
  is_date1 <- as.Date(d_m_Y_to_date_string(day=d10+10*d1,month=m_int,year=Y_int), "%d/%m/%Y")
  is_date2 <- as.Date(d_m_Y_to_date_string(day=d_int,month=m10+10*m1,year=Y_int), "%d/%m/%Y")
  
  return(c(is_date1,is_date2))
}

generate_neighbour_substitution_one_digit_as_char <- function(date_d, position=1, kb_type="upper")
{
  subs_mat <- matrix(rep(0,20),ncol=10)
  
  subs_mat[1,] <- c(as.character(c(2:9,0)),"*")
  subs_mat[2,] <- c("§",as.character(c(1:9)))
  
  #browser()
  
  digit <- as.integer(substr(as.character(date_d, format="%d/%m/%Y"),start=position, stop = position))
  
  if(digit==0) digit <- 10
  
  s1 <- NA
  s2 <- NA
  
  if(digit<10){
    s1 <- as.character(date_d, format="%d/%m/%Y")
    substr(s1,start=position, stop = position) <- subs_mat[1,digit]
  }
  
  if(digit>1){
    s2 <- as.character(date_d, format="%d/%m/%Y")
    substr(s2,start=position, stop = position) <- subs_mat[2,digit]
  }
  
  return(c(s1,s2))
  #return(c(as.Date(s1, "%d/%m/%Y"),as.Date(s2, "%d/%m/%Y")))
}

generate_neighbour_substitution <- function(date_d, kb_type = "upper")
{
  pos <- c(1,2,4,5,7,8,9,10)
  res <- NULL
  
  for(p in pos){
    res <- c(res,generate_neighbour_substitution_one_digit_as_char(date_d=date_d,position=p,kb_type=kb_type))
    #browser()
  }
  return(as.Date(res,format="%d/%m/%Y"))
}

calculate_date_matrix <- function(start_date, end_date, p_error, format_date)
{
  #sequence of potential dates
  potential_dates <- seq(from=start_date, to=end_date, by=1)
  
  #calculate the number of days in the date space
  length_period <- length(potential_dates)
  
  #initialise the matrix with the random given probabilities divided by the size of the date space
  likelihood_error_date <- matrix(rep(0,length_period^2),ncol=length_period)
  
  for(d in 1:length_period)
  {
    #browse through the date entries
    date_d <- potential_dates[d]
    
    ####################### external swap #######################
    
    #generates the potential external swap error dates
    es_date <- generate_external_swap(date_d)
    
    #generates and adds the probabilities in the likelihood matrix
    likelihood_error_date[,d] <- likelihood_error_date[,d] + generate_vector_prob_dates(date_set = es_date, date_space = date_d, prob=p_error$external_swap)
    
    ####################### internal swap #######################
    
    #generates the potential external swap error dates
    is_date <- generate_internal_swap(date_d)
    
    #generates and adds the probabilities in the likelihood matrix
    likelihood_error_date[,d] <- likelihood_error_date[,d] + generate_vector_prob_dates(date_set = es_date, date_space = date_d, prob=p_error$internal_swap)
    
    ####################### neigbour substitution #######################
    
    #generates the potential external swap error dates
    ns_date <- generate_neighbour_substitution(date_d)
    
    #generates and adds the probabilities in the likelihood matrix
    likelihood_error_date[,d] <- likelihood_error_date[,d] + generate_vector_prob_dates(date_set = ns_date, date_space = date_d, prob=p_error$neighbour_substitution)
    
    
  }
  
  return(likelihood_error_date)
  
}

list_errors <- list(external_swap=.04,internal_swap=.005,neighbour_substitution=0.01,distant_substitution=0.02,random=0.01)

#take the dates from date_set (potentially NA) and return a vector mapping the date_space vector with probabilities
#such that the the elements in the date_set (including each NA) have the same probabilities summing to
generate_vector_prob_dates <- function(date_set, date_space, prob){
  
  #Count the number of element in the set
  n_d_set <- length(date_set)
  
  #test if the elements from the set belong to the date space
  present_in_d_space <- (date_set %in% date_space)
  
  #for each element of the set present in the space, return their respective position in the date space
  v <- sapply(date_set[present_in_d_space], FUN = function(x){return(which(date_space==x))})
  
  #for each element in the date space, produces how many times it is present in the date_set
  result <- tabulate(v, nbins = length(date_space))
  
  #return the probability increment for the error matrix
  #given by the frequency in the set (v), the overall probability (prob) divided by the number of elements (n_d_set)
  return(result*prob/n_d_set)
}

# d_s <- seq(from =as.Date("28/12/2019", "%d/%m/%Y"),length.out = 365, by = 1)
# 
# s_to_test <- generate_neighbour_substitution(as.Date("03/02/2020", "%d/%m/%Y"), kb_type = "upper")
# 
# res_v <- generate_vector_prob_dates(date_set = s_to_test, date_space = d_s, prob=0.05)
# 
# names(res_v) <- d_s
# 
# res_v[res_v>0]

#mat_e_d <- calculate_date_matrix(start_date=as.Date("28/12/2019", "%d/%m/%Y"),end_date=as.Date("28/12/2020", "%d/%m/%Y"), p_error=list_errors)