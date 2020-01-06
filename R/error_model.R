source("R/functions.R")

calculate_date_matrix <- function(start_date, end_date, p_error, format_date)
{
  potential_dates <- seq(from=start_date, to=end_date, by=1)
  
  length_period <- as.integer(end_date - start_date)
  
  likelihood_error_date <- matrix(rep(0,length_period^2),ncol=length_period)
  
  for(d in 1:length_period)
  {
    #date_string <- as.character(d, format=format_date)
    
    date_d <- potential_dates[d]
    
    d_int <- as.integer(as.character(date_d, format="%d"))
    m_int <- as.integer(as.character(date_d, format="%m"))
    Y_int <- as.integer(as.character(date_d, format="%Y"))
    
    #external swap
    
    es_date <- as.Date(d_m_Y_to_date_string(day=m_int,month=d_int,year=Y_int), "%d/%m/%Y")
    
    #browser()
    
    if(!is.na(es_date))
    {
      es_date_pos <- which(es_date==potential_dates)
      #browser()
      likelihood_error_date[d,es_date_pos] <- likelihood_error_date[d,es_date_pos] + p_error
    }
    
    #internal swap
    
    #day swap
    day_digit_1 <- d_int/10
    day_digit_2 <- d_int/10
    
   
    
    #likelihood_error_date[d,es_date_pos]
    
  }
  
  return(likelihood_error_date)
  
}

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

generate_neighbour_substitution_one_digit <- function(date_d, position=1, kb_type="upper")
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
    s1 <- as.character(date_d)
    substr(s1,start=position, stop = position) <- subs_mat[1,digit]
  }
  
  if(digit>1){
    s2 <- as.character(date_d)
    substr(s2,start=position, stop = position) <- subs_mat[2,digit]
  }
  
  return(c(as.Date(s1, "%d/%m/%Y"),as.Date(s2, "%d/%m/%Y")))
}

generate_neighbour_substitution <- function(date_d, kb_type = "upper")
{
  pos <- c(1,2,4,5,7,8,9,10)
  res <- NULL
  
  for(p in pos){
    res <- c(res,generate_neighbour_substitution_one_digit(date_d=date_d,position=p,kb_type=kb_type))
  }
  return(res)
}

#mat_e_d <- calculate_date_matrix(start_date=as.Date("28/12/2019", "%d/%m/%Y"),end_date=as.Date("28/12/2020", "%d/%m/%Y"), p_error=.04)