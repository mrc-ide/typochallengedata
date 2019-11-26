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
    if(mutation_flag==2) return("random_substitution")
  }
  return("non_identified")
}

recover_date <- function(date)
{
  re <- "^'?([0-9]+)([[:punct:]]+)([0-9]+)([[:punct:]]+)([0-9]+)[[:punct:]]*$"
  
  day <- as.numeric(sub(re, "\\1", date))
  month <- as.numeric(sub(re, "\\3", date))
  year <- as.numeric(sub(re, "\\5", date))
  
  return(d_m_Y_to_date_string(day,month,year))
}
