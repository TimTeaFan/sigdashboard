# function to create a simulation data.frame

create_df <- function(n, n2 = NULL, base, target) {
  
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
  
  df %>% 
    group_by(group) %>%
    count(value) %>%
    mutate(freq = prop.table(n)) %>%
    filter(group == "new", value == 1) %>% 
    pull(freq)
}


# function to test for significance

sig_test <- function(df) {
  
  df %>% 
    lm(value ~ group, data = .) %>% 
    summary %>% 
    broom::tidy() %>% 
    filter(term == "groupref") %>% 
    pull(p.value)
  
}


