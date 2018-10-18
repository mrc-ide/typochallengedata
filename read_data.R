date_of_contributions <- "20181018"

dat <- readRDS(paste0("./contributions/", date_of_contributions, "/contributions.rds"))

n <- vapply(dat, function(x) nrow(x$data), integer(1))
tmp <- do.call("rbind", lapply(dat, "[[", "data"))
ret <- cbind(id = rep(names(dat), n), tmp, stringsAsFactors = FALSE)
rownames(ret) <- NULL

######################################################
### Characteristics of contributions ###
######################################################

### total number of entries ###
nrow(ret)

### number of contributions ###
contributions <- names(dat)
n_contributions <- length(contributions)
n_contributions

### Number of entries by contribution ### 
hist(table(ret$id), breaks = seq(0, 400, 10), col = "grey", 
     main = "", xlab = "Number of entry by contribution")

mean(table(ret$id))
median(table(ret$id))
sd(table(ret$id))
range(table(ret$id))

### TO DO: ADD ANALYSIS OF SURVEY DATA HERE ###

######################################################
### Characteristics of errors ###
######################################################

### frequency of error (across all participants) ###
prop_error <- table(ret$correct)['FALSE'] / sum(table(ret$correct))

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



