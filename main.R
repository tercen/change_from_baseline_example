library(tercen)
library(dplyr)

do.bc = function(df, percentage_format) {
  

  base_tbl <- df %>% group_by(.axisIndex) %>% 
     summarise(value = mean(.y)) %>% 
     filter(.axisIndex == 1)
  baseline <- base_tbl$value
  
  if (length(baseline) == 0) baseline = NaN
  
  data_tbl <- df %>% group_by(.axisIndex) %>% 
    summarise(value = mean(.y)) %>% 
    filter(.axisIndex == 0)
  data <- data_tbl$value
  
  if (length(data) == 0) data = NaN
   
  change <- (data - baseline) / baseline
  
  if (!is.na(data) && !is.na(baseline)) {
    if ((data == 0) &&  (baseline == 0))
      change <- 0
  }
   
  if (percentage_format == TRUE) change <- (change * 100)

  return(data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    change = change
  ))
}

ctx = tercenCtx()

if (nrow(unique(ctx$select(c('.axisIndex')))) != 2)
  stop("Two layers are required, one with the data value and with the baseline value.")


ctx %>%
  select(.ci, .ri, .y, .axisIndex) %>%
  group_by(.ci, .ri) %>%
  do(do.bc(., as.logical(ctx$op.value('percentage')))) %>%
  ctx$addNamespace() %>%
  ctx$save()
