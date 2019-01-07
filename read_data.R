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

######################################################
### Characteristics of participants ###
######################################################

countries_from <- unlist(sapply(1:length(dat), function(e) dat[[e]]$survey$country_from))
table(countries_from)

countries_residence <- unlist(sapply(1:length(dat), function(e) dat[[e]]$survey$country_residence))
table(countries_residence)

