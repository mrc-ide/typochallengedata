compute_freq_error_per_contribution <- function(id) {
  table(ret[ret$id %in% id,]$correct)['FALSE'] / 
    sum(table(ret[ret$id %in% id,]$correct))
}

#################################
### error functions ###
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

### day month swapped ###
is_day_month_swapped <- function(date1_char, date2_char) # TO DO: this does not allow for missing zeros (i.e. typing "6" instead of "06")
{
  extract_day(date1_char) == extract_month(date2_char) &&
     extract_day(date2_char) == extract_month(date1_char)
}
