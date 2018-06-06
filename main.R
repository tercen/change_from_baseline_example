library(tercen)
library(dplyr)

do.bc = function(df, percentage_format) {
  

  base_tbl <- df %>% group_by(.axisIndex) %>% 
     summarise(value = mean(.y)) %>% 
     filter(.axisIndex == 1)
  base <- base_tbl$value
  
  if (length(base) == 0) base = NaN
  
  data_tbl <- df %>% group_by(.axisIndex) %>% 
    summarise(value = mean(.y)) %>% 
    filter(.axisIndex == 0)
  data <- data_tbl$value
  
  if (length(data) == 0) data = NaN
   
  basechange <- (data-base)/base
  
  if ((data == 0) &&  (base ==0)) basechange <- 0
   
  if (percentage_format == TRUE) basechange <- (basechange * 100)

  return(data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    basechange = basechange
  ))
}

ctx = tercenCtx()

if (nrow(unique(ctx$select(c('.axisIndex')))) != 2)
  stop("Two layers are required, one with the data value and with the base value")


ctx %>%
  select(.ci, .ri, .y, .axisIndex) %>%
  group_by(.ci, .ri) %>%
  do(do.bc(., as.logical(ctx$op.value('percentage')))) %>%
  ctx$addNamespace() %>%
  ctx$save()
