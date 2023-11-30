# print variable names ordered by their missingness rates
check_missing_rate = function(df){
  missinginfo <- df %>%
    gather(key = "key", value = "val") %>%
    mutate(is.missing = is.na(val)) %>%
    group_by(key, is.missing) %>%
    summarise(num.missing = n()) %>%
    filter(is.missing==T) %>%
    select(-is.missing) %>%
    arrange(desc(num.missing)) 
  missinginfo$missing.rate=missinginfo$num.missing/nrow(df)
  print(missinginfo)
}

# plot the missingness rate for each variable
plot_missing_rate = function(df){
  missing.values <- df %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)
  
  #levels <- (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
  levels = colnames(df)
  
  percentage.plot <- missing.values %>%
    ggplot() +
    geom_bar(aes(x = factor(key, levels=levels), #reorder(key, desc(pct)), 
                 y = pct, fill=isna), 
             stat = 'identity', alpha=0.8) +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(name = "", 
                      values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
    coord_flip() +
    labs(title = "Percentage of missing values", x =
           'Variable', y = "% of missing values")
  
  percentage.plot
}