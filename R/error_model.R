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
    browser()
  }
  return(as.Date(res,format="%d/%m/%Y"))
}

calculate_date_matrix <- function(start_date, end_date, p_error, format_date)
{
  #sequence of potential dates
  potential_dates <- seq(from=start_date, to=end_date, by=1)
  
  length_period <- as.integer(end_date - start_date)
  
  likelihood_error_date <- matrix(rep(0,length_period^2),ncol=length_period)
  
  for(d in 1:length_period)
  {
    #browse through the date entries
    date_d <- potential_dates[d]
    
    #generate the potential external swap error
    es_date <- generate_external_swap(date_d)
    
    if(!is.na(es_date))
    {
      es_date_pos <- which(es_date==potential_dates)
      #browser()
      likelihood_error_date[d,es_date_pos] <- likelihood_error_date[d,es_date_pos] + p_error$external_swap
    }
    
    #internal swap
    
    
    
    
    #likelihood_error_date[d,es_date_pos]
    
  }
  
  return(likelihood_error_date)
  
}

list_errors <- list(external_swap=.04,internal_swap=.005,neighbour_mutation=0.01,distant_mutation=0.02,random=0.01)

#take the dates from date_set (potentially NA) and return a vector mapping the date_space vector with probabilities
#such that the the elements in the date_set (including each NA) have the same probabilities summing to prob
generate_vector_prob_dates <- function(date_set, date_space, prob){
  
}

#mat_e_d <- calculate_date_matrix(start_date=as.Date("28/12/2019", "%d/%m/%Y"),end_date=as.Date("28/12/2020", "%d/%m/%Y"), p_error=list_errors)