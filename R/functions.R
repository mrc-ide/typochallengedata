compute_freq_error_per_contribution <- function(id) {
  table(ret[ret$id %in% id,]$correct)['FALSE'] / 
    sum(table(ret[ret$id %in% id,]$correct))
}

### error functions ###

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

is_day_month_swapped <- function(date1_char, date2_char) # TO DO: this does not allow for missing zeros (i.e. typing "6" instead of "06")
{
  extract_day(date1_char) == extract_month(date2_char) &&
     extract_day(date2_char) == extract_month(date1_char)
}