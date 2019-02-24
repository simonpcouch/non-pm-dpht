# `calc_stat` takes in the output of `gen_data` and calls the `kw` or `kw_new` 
# function with the correct arguments for each unique `replicate` value
# depending on the attributes of the input. The output is a distribution of 
# test statistics.

calc_stat <- function(data) {
  if (data[[2]] == "abs" & "noise" %in% colnames(data[[1]])) { 
    # carry out the private abs version
    data[[1]] %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = kw_new(x, group, public = FALSE) + noise[1]) %>%
      dplyr::select(stat) %>%
      pull()
  } else if (data[[2]] == "sq" & "noise" %in% colnames(data[[1]])) { 
    # carry out the private sq version
    data[[1]] %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = kw(x, group, public = FALSE) + noise[1]) %>%
      dplyr::select(stat) %>%
      pull()    
  } else if (data[[2]] == "sq") { 
    # carry out the public sq version
    data[[1]] %>% 
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = kw(x, group, public = TRUE)) %>%
      dplyr::select(stat) %>%
      pull()
  } else if (data[[2]] == "abs") { 
    # carry out the public abs version
    data[[1]] %>% 
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = kw_new(x, group, public = TRUE)) %>%
      dplyr::select(stat) %>%
      pull()
  }
}
