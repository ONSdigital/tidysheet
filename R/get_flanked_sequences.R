#' @title get_first_data_row
#' @description  Get the number of the row that contains the first line of data
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Identify consecutive runs of numbers that are flanked on both sides by 
#' numbers in another sequence. 
#' 
#' @param flank_nums integer vector. 
#' @param target_nums integer vector. Numbers that   
#' 
#' @examples
#' flank_nums <- c( 5, 6, 8, 9, 12, 13, 15)
#' target_nums <- c(10, 14:15)
#' # missing numbers 7 and 11 from both series
#' # so result will be 13, 14, 15 
#' get_flanked_sequences(flank_nums, target_nums)
#' @export
get_flanked_sequences <- function(flank_nums, target_nums) {
  
  flank_nums <- sort(flank_nums)
  target_nums <- sort(target_nums)
  
  consecutives <- split_by_consecutives(target_nums)
  flanking_ranges <- split_by_consecutives(flank_nums)
  
  first_target <- sapply(consecutives, "[[", 1)
  last_target <- sapply(consecutives, tail, 1)
  
  first_flank <- sapply(flanking_ranges, "[[", 1)
  last_flank <- sapply(flanking_ranges, tail, 1)
  
  flank_df <- data.frame("first" = first_flank, 
                         "last" = last_flank,
                         "type" = "flank")
  
  target_df <- data.frame("first" = first_target, 
                          "last" = last_target,
                          "type" = "target")
  
  all_targets <- data.frame("all" = target_nums)
  
  # find which sequences of target numbers are bound on either side by 'flank'
  # numbers
  flanked_targets <- flank_df %>% 
    bind_rows(target_df) %>% 
    arrange(first) %>% 
    # there may be numbers that are in neither target_nums nor flank_nums so
    # we can't just rely on arrange, and need to check if "first" in each row 
    # directly follows "last" in the previous row.
    mutate(follows_previous = lag(last)==first-1,
           next_follows = lead(first)==last+1) %>% 
    mutate(flanked_target = 
             ifelse(follows_previous == TRUE &
                      next_follows == TRUE &
                      type == "target",
                    TRUE, FALSE)) %>% 
    filter(flanked_target==TRUE)
  
  row.names(flanked_targets) <- NULL
  
  # flanked_targets contains ranges (first:last) of the rows we need to 
  # identify, not the numbers of all the rows to identify. We therefore need to 
  # identify all the target rows between first and last for which we can use the 
  # vector of all row numbers identified as blank 
  all_in_flanked_sequence <- all_targets %>% 
    expand_grid(flanked_targets) %>% 
    filter(all >= first & all <= last) %>% 
    pull(all)
  
  return(all_in_flanked_sequence)
}
