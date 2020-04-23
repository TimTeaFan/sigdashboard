# function to create a simulation data.frame

create_df <- function(n, base, target, n2 = NULL) {
  
  if (is.null(n2)) {
    
    tibble(group = c(rep("ref", n), rep("new", n)),
           value = c(rep(1, round(n * base, 0)),
                     rep(0, n - round(n * base, 0)),
                     rep(1, round(n * target, 0)),
                     rep(0, n - round(n * target, 0))))
    
  } else {
    
    tibble(group = c(rep("ref", n), rep("new", n2)),
           value = c(rep(1, round(n * base, 0)),
                     rep(0, n - round(n * base, 0)),
                     rep(1, round(n2 * target, 0)),
                     rep(0, n2 - round(n2 * target, 0))))
    
  }
}
  
# functions to check real percentage of respondents
check_df <- function(df) {
  
  res <- df %>% 
    group_by(group) %>%
    count(value) %>%
    mutate(freq = prop.table(n)) %>%
    filter(group == "new", value == 1)
  
  if (nrow(res) > 0) return(pull(res, freq))
    else return(NA)
  
}


# function to test for significance

sig_test <- function(df, chi2 = TRUE) {
  
  if (chi2) {
    
   temp <- table(df[["group"]], df[["value"]]) %>%
              chisq.test %>% 
              broom::tidy() 
    
  } else {
    
    temp <- t.test(value ~ group, data = df) %>% 
              broom::tidy()
  }
  
    pull(temp, p.value)
  
}


# sig_test_old <- function(df) {
# 
#   df %>%
#     lm(value ~ group, data = .) %>%
#     summary %>%
#     broom::tidy() %>%
#     filter(term == "groupref") %>%
#     pull(p.value)
# 
# }


# find sample size

find_sample_size <- function(.n1 = NULL, .n2 = 5000, .base, .target, chi2 = TRUE) {
  
  i = .n2
  n_ini <- seq(i/50, i, by = .5*i / 50)
  check <- 0
  
  if (check == 0) {
    
  temp_res <- tibble(n = n_ini) 
  
    if (is.null(.n1)) {
    temp_res <- temp_res %>%
      mutate(data = map(n, ~ create_df(n = .x, n2 = .x, base = .base, target = .target)))
    } else {
      temp_res <- temp_res %>%
        mutate(data = map(n, ~ create_df(n = .n1, n2 = .x, base = .base, target = .target)))
    }
  
  temp_res <- temp_res %>%
    mutate(real_target = map_dbl(data, check_df),
           p_value = map_dbl(data, ~ sig_test(.x, chi2)),
           sig_level = case_when(
             p_value < .001 ~ 0.1,
             p_value < .01 ~ 1,
             p_value < .05 ~ 5,
             p_value < .1  ~ 10,
             T ~ 100)
    ) %>% 
    select(n, real_target, p_value, sig_level)
  
  check <- any(temp_res$p_value < .05)
  
  }
  
  if (check) {

    res_n <- temp_res %>%
      filter(rev(cummax(rev(p_value))) < .05 & cummin(p_value) <= .05) %>% 
      filter(p_value == max(p_value)) %>%
      pull(n)

    res_ls <- list(data = temp_res,
                   n = res_n)
    
    return(res_ls)
  
   } else if (check == 0 & i < 20000) {

    i <- i + 5000
    find_sample_size(.n1 = .n1, .n2 = i, .base = .base, .target = .target)

   } else {
     res_ls <- list(data = temp_res,
                    n = NULL)
     return(res_ls)
  }

}


# Calculate Difference for all Ranges

calc_diff <- function(.n1, .n2, .diff, chi2 = TRUE) {
  
  if (sign(.diff) == -1) {
    
    target <- 0
    base <- target + .diff 
    
  } else if (sign(.diff) == 1) {
    
    base <- 0
    target <- base + .diff 
    
  } else if (.diff == 0) {
    stop("difference cannot be `0`")
  } 
  
  tibble(target = seq(target_range1(),
                      target_range2(),
                      by = steps())) %>%
    mutate(data = map(target,
                      ~ create_df(n = input$n1,
                                  n2 = n2_reac(),
                                  base = base(),
                                  target = .x)),
           real_target = map_dbl(data, check_df),
           p_value = map_dbl(data, ~ sig_test(.x, input$chi2)),
           sig_level = case_when(
             p_value < .001 ~ "0.1 %",
             p_value < .01 ~ "1 %",
             p_value < .05 ~ "5 %",
             p_value < .1  ~ "10 %",
             T ~ "Not sig.")
    ) %>%
    select(target, real_target, p_value, sig_level)
  
  
}



