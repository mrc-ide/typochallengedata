######################################################
### read contributions ###
######################################################

date_of_contributions <- "Final"

dat <- readRDS(paste0("./contributions/", date_of_contributions, "/contributions.rds"))

n_entries_per_contribution <- vapply(dat, function(x) nrow(x$data), integer(1))
tmp <- do.call("rbind", lapply(dat, "[[", "data"))
ret <- cbind(id = rep(names(dat), n_entries_per_contribution), tmp, stringsAsFactors = FALSE)
rownames(ret) <- NULL

######################################################
### source functions ###
######################################################
source("R/functions.R")

######################################################
### Characteristics of contributions ###
######################################################

### total number of entries ###
nrow(ret)

### number of contributions ###
id_contributions <- names(dat)
n_contributions <- length(id_contributions)
n_contributions

### Number of entries by contribution ### 
hist(table(ret$id), breaks = seq(0, 460, 10), col = "grey", 
     main = "", xlab = "Number of entry by contribution")

mean(table(ret$id))
median(table(ret$id))
sd(table(ret$id))
range(table(ret$id))

### TO DO: ADD ANALYSIS OF SURVEY DATA HERE ###


######################################################
### POTENTIAL ISSUE: DIFFERENCES IN ID RECORDED? ###
######################################################

# ids_1 <- sort(id_contributions) # defined from names(dat)
# is not the same as:
# ids_2 <- sort(unique(ret$id))
# length(ids_1)
# length(ids_2)

######################################################
### Characteristics of errors ###
######################################################

### frequency of error (across all contributions) ###
freq_error <- table(ret$correct)['FALSE'] / sum(table(ret$correct))

### frequency of error (per contribution) ###
freq_error_per_contribution <- 
  sapply(id_contributions, compute_freq_error_per_contribution)

par(mfrow=c(1, 2))
hist(freq_error_per_contribution, breaks = seq(0, 1, 0.05), col = "grey", 
     main = "", xlab = "Frequency of error by contribution")
plot(n_entries_per_contribution, freq_error_per_contribution,
     col = scales::alpha("black", 0.5),
     xlab = "Number of entries", ylab = "Frequency of error")

### looking at the errors made ###
erroneous_ret <- ret[!ret$correct,]

head(erroneous_ret, 20)

### TO DO: ADD FUNCTION TO DETECT FOLLOWING ERRORS: ###
### these are the error types identified by eye in the first 100 entries:
# empty entry
# wrong separator (everywhere or somewhere)
# one digit wrongly entered 
#     --> distinguish neighbouring digit and other digits
#     --> see if any particular couple of digits are often mistaken for one another (e.g. 1 and 7)
# one number always wrongly entered (if handwritten) e.g. 27/7 instead of 29/9
# two successive digits wrongly entered
# additional character
# double separator somewhere
# two successive digits swapped
# two successive digits swapped AND an error in one of them
# day month swapped
# day month swapped AND one digit wrongly entered
# thirty vs thirteen
# month wrong ("September" vs 11 (! --> seen twice in first 100 entries))
# first two digits of the year skipped

#########

date1_char <- erroneous_ret$date # true date
date2_char <- erroneous_ret$user # date entered by user

# regular expression to track day sep1 month sep2 year pattern
# ?grep ?regex
re <- "^'?([0-9]+)([[:punct:]]+)([0-9]+)([[:punct:]]+)([0-9]+)[[:punct:]]*$"
recognised_by_regex <- grepl(re, date2_char)

# empty entries
empty_entries <- date2_char == ""

# fully spelt out date (with space or punctuation to separate dates)
re2 <- "^'?([0-9]+)([[:space:][:punct:]]+)([[:alpha:]]+)([[:space:][:punct:]]+)([0-9]+)[[:punct:]]*$"
full_spell_out_entries <- grepl(re2, date2_char)
date2_char[full_spell_out_entries]

# missing one field (only one separator)
re3 <- "^'?([0-9]+)([[:punct:]]+)([0-9]+)[[:punct:]]*$"
missing_one_numeric_field <- grepl(re3, date2_char)

# missing two fields (no separator)
re4 <- "^'?([0-9]+)[[:punct:]]*$"
missing_two_numeric_fields <- grepl(re4, date2_char)

### what are we currently not capturing?  
captured <- recognised_by_regex | empty_entries | full_spell_out_entries | missing_one_numeric_field | missing_two_numeric_fields
date2_char[!captured] 
date1_char[!captured] 
prop.table(table(captured)) # <1% of errors not captured by current algorithm --> can look at these manually really!
# table(recognised_by_regex , empty_entries , full_spell_out_entries , missing_one_numeric_field, missing_two_numeric_fields)

### for those recognised by regular expressions, get day, month, year out of there ###
x <- date2_char[recognised_by_regex]
list(day = as.numeric(sub(re, "\\1", x)),
     sep1 = sub(re, "\\2", x),
     month = as.numeric(sub(re, "\\3", x)),
     sep2 = sub(re, "\\4", x),
     year = as.numeric(sub(re, "\\5", x)))

four_digit_year <- nchar(sub(re, "\\5", x)) == 4

#res <- logical(length(recognised_by_regex))
#res[!recognised_by_regex] <- NA
#res[recognised_by_regex][four_digit_year] <- TRUE

######################################################
### Characteristics of participants ###
######################################################

countries_from <- unlist(sapply(1:length(dat), function(e) dat[[e]]$survey$country_from))
table(countries_from)

countries_residence <- unlist(sapply(1:length(dat), function(e) dat[[e]]$survey$country_residence))
table(countries_residence)

