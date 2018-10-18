date_of_contributions <- "20181018"

dat <- readRDS(paste0("./contributions/", date_of_contributions, "/contributions.rds"))

n <- vapply(dat, function(x) nrow(x$data), integer(1))
tmp <- do.call("rbind", lapply(dat, "[[", "data"))
ret <- cbind(id = rep(names(dat), n), tmp, stringsAsFactors = FALSE)
rownames(ret) <- NULL


