compute_freq_error_per_contribution <- function(id) {
  table(ret[ret$id %in% id,]$correct)['FALSE'] / 
    sum(table(ret[ret$id %in% id,]$correct))
}

#################################
### error functions - ALL THE BELOW NEEDS REWRITING  USING RICH'S APPROACH WITH REGULAR EXPRESSIONS I THINK ###
#################################

### handy functions ###
extract_day <- function(date_char)
{
  strsplit(date_char, "/")[[1]][1]
}

extract_month <- function(date_char)
{
  strsplit(date_char, "/")[[1]][2]
}

extract_year <- function(date_char)
{
  strsplit(date_char, "/")[[1]][3]
}

extract_sep <- function(date_char)
{
  tmp <- as.vector(gregexpr("[0-9]", date_char)[[1]])
  pos_sep <- setdiff(seq_len(nchar(date_char)), tmp)
  sapply(pos_sep, function(e) substr(date_char, start = e, stop = e))
}

### empty_entry ###
is_empty <- function(date_char)
{
  date_char == ""
}

### too many / too few separators ###
is_n_sep_wrong <- function(date_char)
{
  tmp <- extract_sep(date_char)
  if(length(tmp) != 2) TRUE else FALSE
}

### wrong separator (assuming correct number of separators) ###
is_n_sep_wrong <- function(date_char)
{
  tmp <- extract_sep(date_char)
  if(length(tmp) != 2) TRUE else FALSE
}

### day month swapped ###
is_day_month_swapped <- function(date1_char, date2_char) # TO DO: this does not allow for missing zeros (i.e. typing "6" instead of "06")
{
  extract_day(date1_char) == extract_month(date2_char) &&
     extract_day(date2_char) == extract_month(date1_char)
}

is_internal_swap <- function(date1, date2){
  day_internal_swap <- paste0(substr(date1,2,2),substr(date1,1,1),substr(date1,3,10))
  if(day_internal_swap==date2) return(TRUE)
  month_internal_swap <- paste0(substr(date1,1,3),substr(date1,5,5),substr(date1,4,4),substr(date1,6,10))
  if(month_internal_swap==date2) return(TRUE) else return(FALSE)
}


is_external_swap <- function(date1, date2){
  external_swap_date <- paste0(substr(date1,4,6),substr(date1,1,3),substr(date1,7,10))
  if(external_swap_date==date2) return(TRUE) else return(FALSE)
}

#Return a vector with TRUE if the character at the position are the same FALSE if different 
#! the tested string may be of the same length
differing_position <- function(date1, date2) {
  strsplit(date1, NULL)[[1]] != strsplit(date2, NULL)[[1]]
}

is_neighbouring_swap <- function(original_date, recorded_date) {
  
  #test that the two strings have the same length
  if(nchar(original_date)!=nchar(recorded_date)) return(FALSE)
  
  diff_vec <- differing_position(original_date, recorded_date)
  
  if(sum(diff_vec)!=1) return(FALSE) else{
    
    mutation_position <- which(diff_vec)
    
    if(mutation_position %in% c(3,6)) return(FALSE)
    
    #browser()
    
    num_char_set <- c("0","1","2","3","4","5","6","7","8","9")
    if(!(substr(recorded_date,mutation_position,mutation_position) %in% num_char_set)) return(FALSE)
    
    original_digit <- as.numeric(substr(original_date,mutation_position,mutation_position))
    recorded_digit <- as.numeric(substr(recorded_date,mutation_position,mutation_position))
    
    if(is.na(recorded_digit)) return(FALSE)
    
    #######  is a swap from "0" to "-" considered a neighbouring swap? ; probably not
    
    if((original_digit<9)&(recorded_digit-original_digit==1)) return(TRUE)
    if((original_digit>1)&(original_digit-recorded_digit==1)) return(TRUE)
    if((original_digit==9)&(recorded_digit==0)) return(TRUE)
    if((original_digit==0)&(recorded_digit==9)) return(TRUE) else
      return(FALSE)
  }
}

#0 not "mutation"
#1 neighbouring swap
#2 random swap
is_mutation <- function(original_date, recorded_date) {
  
  #test that the two strings have the same length
  if(nchar(original_date)!=nchar(recorded_date)) return(0)
  
  diff_vec <- differing_position(original_date, recorded_date)
  
  if(sum(diff_vec)!=1) return(0) else{
    
    mutation_position <- which(diff_vec)
    
    if(mutation_position %in% c(3,6)) return(0)
    
    #browser()
    
    num_char_set <- c("0","1","2","3","4","5","6","7","8","9")
    if(!(substr(recorded_date,mutation_position,mutation_position) %in% num_char_set)) return(0)
    
    original_digit <- as.numeric(substr(original_date,mutation_position,mutation_position))
    recorded_digit <- as.numeric(substr(recorded_date,mutation_position,mutation_position))
    
    #######  is a swap from "0" to "-" considered a neighbouring swap? ; probably not
    
    if((original_digit<9)&(recorded_digit-original_digit==1)) return(1)
    if((original_digit>1)&(original_digit-recorded_digit==1)) return(1)
    if((original_digit==9)&(recorded_digit==0)) return(1)
    if((original_digit==0)&(recorded_digit==9)) return(1) else
      return(2)
  }
}

#takes 3 integers and transform it to a date string, the date does not have to be valid
#at the moment d_m_Y_to_date_string(12,36,20955) will return "12/36/20955" ;  test for year made of 4 digits?
d_m_Y_to_date_string <- function(day,month,year)
{
  d1 <- as.character(day %/% 10)
  d2 <- as.character(day %% 10)
  d4 <- as.character(month %/% 10)
  d5 <- as.character(month %% 10)
  d7 <- as.character(year %/% 1000)
  d8 <- as.character((year %% 1000) %/% 100 )
  d9 <- as.character((year %% 100) %/% 10)
  d10 <- as.character(year %% 10)
  
  return(paste0(d1,d2,"/",d4,d5,"/",d7,d8,d9,d10))
}

error_type <- function(original_date, recorded_date) {
  if(is_internal_swap(original_date,recorded_date)){
    return("internal_swap") }
  
  if(is_external_swap(original_date,recorded_date)){
    return("external_swap") }
  
  mutation_flag <- is_mutation(original_date,recorded_date)
  if(mutation_flag != 0){
    if(mutation_flag==1) return("neighbour_substitution") 
    if(mutation_flag==2) return("distant_substitution")
  }
  return("random")
}

recover_date <- function(date)
{
  re <- "^'?([0-9]+)([[:punct:]]+)([0-9]+)([[:punct:]]+)([0-9]+)[[:punct:]]*$"
  
  day <- as.numeric(sub(re, "\\1", date))
  month <- as.numeric(sub(re, "\\3", date))
  year <- as.numeric(sub(re, "\\5", date))
  
  return(d_m_Y_to_date_string(day,month,year))
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

generate_distant_substitution_one_digit_as_char <- function(date_d, position=1, kb_type="upper")
{
  neighbour_mat <- matrix(rep(0,20),ncol=10)
  
  #browser()
  
  #the digits, "*" codes for the character at the right of the "0" and "§" for the character at the left of the "1"
  #TODO is it relevant to count the outside digits as potential substitutions? probably not
  potential_substitutions <- c(as.character(1:9),"0","*","§")
  
  #Stores which character are the neighbouring one so that can be removed from the substitution scheme at the relevant position
  neighbour_mat[1,] <- c(2:9,10,11)
  neighbour_mat[2,] <- c(12,1:9)
  
  #browser()
  
  #read the value of the digit to be substituted
  digit <- as.integer(substr(as.character(date_d, format="%d/%m/%Y"),start=position, stop = position))
  
  #as the velue is used for the position in the neighbour matrix, 10 is used instead of 0
  if(digit==0) digit <- 10
  
  #for each digit, 9 substitutions are possible (the 12 digits minus the digit and the two neighbours) 
  #we work through the set by removing the digit and the neighbours
  distant_substitution_set <- potential_substitutions[-c(digit,neighbour_mat[,digit])]
  
  result <- NULL
  for(s in distant_substitution_set){
    if((s=="*")| (s=="§")) {
      s_error <- NA}else{
        s_error <- as.character(date_d, format="%d/%m/%Y")
        substr(s_error,start=position, stop = position) <- s
      }
    result <- c(s_error,result)
  }
  
  return(result)
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

generate_distant_substitution <- function(date_d, kb_type = "upper")
{
  pos <- c(1,2,4,5,7,8,9,10)
  res <- NULL
  
  for(p in pos){
    res <- c(res,generate_distant_substitution_one_digit_as_char(date_d=date_d,position=p,kb_type=kb_type))
    #browser()
  }
  return(as.Date(res,format="%d/%m/%Y"))
}

calculate_date_matrix <- function(start_date, end_date, p_error, format_date)
{
  #sequence of potential dates
  potential_dates <- seq(from=start_date, to=end_date, by=1)
  
  #browser()
  
  #calculate the number of days in the date space
  length_period <- length(potential_dates)
  
  #initialise the matrix with the random given probabilities divided by the size of the date space
  likelihood_error_date <- matrix(rep(p_error$random/length_period,length_period^2),ncol=length_period)
  
  for(d in 1:length_period)
  {
    #browse through the date entries
    date_d <- potential_dates[d]
    
    ####################### external swap #######################
    
    #generates the potential external swap error dates
    es_date <- generate_external_swap(date_d)
    
    #generates and adds the probabilities in the likelihood matrix
    likelihood_error_date[,d] <- likelihood_error_date[,d] + generate_vector_prob_dates(date_set = es_date, date_space = potential_dates, prob=p_error$external_swap)
    
    ####################### internal swap #######################
    
    #generates the potential internal swap error dates
    is_date <- generate_internal_swap(date_d)
    
    #generates and adds the probabilities in the likelihood matrix
    likelihood_error_date[,d] <- likelihood_error_date[,d] + generate_vector_prob_dates(date_set = is_date, date_space = potential_dates, prob=p_error$internal_swap)
    
    ####################### neigbour substitution #######################
    
    #generates the potential neigbour substitution error dates
    ns_date <- generate_neighbour_substitution(date_d)
    
    #generates and adds the probabilities in the likelihood matrix
    likelihood_error_date[,d] <- likelihood_error_date[,d] + generate_vector_prob_dates(date_set = ns_date, date_space = potential_dates, prob=p_error$neighbour_substitution)
    
    ####################### distant substitution #######################
    
    #generates the potential distant substitution error dates
    ds_date <- generate_distant_substitution(date_d)
    
    #generates and adds the probabilities in the likelihood matrix
    likelihood_error_date[,d] <- likelihood_error_date[,d] + generate_vector_prob_dates(date_set = ds_date, date_space = potential_dates, prob=p_error$distant_substitution)
    
  }
  
  return(likelihood_error_date)
  
}

#take the dates from date_set (potentially NA) and return a vector mapping the date_space vector with probabilities
#such that the the elements in the date_set (including each NA) have the same probabilities summing to
generate_vector_prob_dates <- function(date_set, date_space, prob){
  
  #browser()
  
  #Count the number of element in the set
  n_d_set <- length(date_set)
  
  #test if the elements from the set belong to the date space
  present_in_d_space <- (date_set %in% date_space)
  
  #for each element of the set present in the space, return their respective position in the date space
  v <- sapply(date_set[present_in_d_space], FUN = function(x){return(which(date_space==x))})
  
  #test if v is empty in this case v=list() and return a 0 length. In this case return the 0 vector
  if(length(v)==0) return(rep(0,length(date_space)))
  
  #for each element in the date space, produces how many times it is present in the date_set
  result <- tabulate(v, nbins = length(date_space))
  
  #return the probability increment for the error matrix
  #given by the frequency in the set (v), the overall probability (prob) divided by the number of elements (n_d_set)
  return(result*prob/n_d_set)
}

rErrorDate <- function(n, date, date_space, date_transition){
  d <- which(date_space==date)
  draw_dates <- rmultinom(n=1, size=n, prob=date_transition[,d])
  return(rep(date_space,draw_dates))
}

dErrorDate <- function(date1, date2, date_space, date_transition){
  d1 <- which(date_space==date1)
  d2 <- which(date_space==date2)
  return(date_transition[d2,d1])
}
