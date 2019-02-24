# `calc_stat` takes in a dataframe outputted by `gen_data` (or a dataframe 
# not outputted by `gen_data` but with with an identical structure) and 
# supplies the `mw` function with it's arguments for each unique value of 
# `replicate`. The output is a vector of test statistics with length = `reps`.

calc_stat <- function(df) {
  if ("noise" %in% colnames(df)) {
    df %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = mw(x, group) + noise[1]) %>%
      dplyr::select(stat) %>%
      pull()
  } else {
    df %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = mw(x, group)) %>%
      dplyr::select(stat) %>%
      pull()
  }
}