# Myspread function
myspread <- function(df, key, value) {
    # quote key
    keyq <- rlang::enquo(key)
    # break value vector into quotes
    valueq <- rlang::enquo(value)
    s <- rlang::quos(!!valueq)
    df %>% gather(variable, value, !!!s) %>%
        unite(temp, !!keyq, variable) %>%
        spread(temp, value)
}


meanD = function(data, indices){d <- data[indices]; return(mean(d, na.rm =T))}
boot.summaries = function(boot.fun,metric,y,d) {
  # boot.fun = median or mean function
  # metric = median or mean
  # y = column to summarise
  # d = data frame (groupped)
  d = d %>% nest()
  out = d %>%
  mutate(
        boots = map(data, ~boot(data = .[[y]], statistic = boot.fun, R = 9999)),
        boots.ci = map(boots, boot.ci, type = c("bca")),
        estimate = map(data, ~metric(.[[y]], na.rm =T)),
        conf_low = map(boots.ci, ~.[[4]][4]),
        conf_high = map(boots.ci, ~.[[4]][5])
      ) %>%
  left_join(d %>% summarise(n = map(data, tally))) %>%
  select(-c(data, boots, boots.ci)) %>% unnest(cols = c(estimate, conf_low, conf_high, n)) %>% ungroup()
  return(out)
}