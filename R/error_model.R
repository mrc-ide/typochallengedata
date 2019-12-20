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
    
    #likelihood_error_date[d,es_date_pos]
    
  }
  
  return(likelihood_error_date)
  
}

#mat_e_d <- calculate_date_matrix(start_date=as.Date("28/12/2019", "%d/%m/%Y"),end_date=as.Date("28/12/2020", "%d/%m/%Y"), p_error=.04)