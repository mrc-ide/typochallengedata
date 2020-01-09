source("R/functions.R")

list_errors <- list(external_swap=.04,internal_swap=.005,neighbour_substitution=0.01,distant_substitution=0.02,random=0.01)

# d_s <- seq(from =as.Date("28/12/2019", "%d/%m/%Y"),length.out = 365, by = 1)
# 
# s_to_test <- generate_neighbour_substitution(as.Date("03/02/2020", "%d/%m/%Y"), kb_type = "upper")
# 
# res_v <- generate_vector_prob_dates(date_set = s_to_test, date_space = d_s, prob=0.05)
# 
# names(res_v) <- d_s
# 
# res_v[res_v>0]

#mat_e_d <- calculate_date_matrix(start_date=as.Date("28/12/2019", "%d/%m/%Y"),end_date=as.Date("28/12/2020", "%d/%m/%Y"), p_error=list_errors)