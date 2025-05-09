# ==============================================================================
# Title: Token-Based Conjoint (TBC) Utilities
# Author: Numerious Inc.
# Description: 
#   This script provides helper functions for working with Token-Based Conjoint
#   (TBC) designs. It includes:
#     - `test_tbc_data()`: to simulate respondent-level TBC data
#     - `tbc_choice_sets()`: to generate long-format choice sets suitable for 
#       analysis or model estimation
#
# Use case:
#   These functions can be used for testing and preparing TBC experiments,
#   especially when integrating forced choice and dual response formats.
#
# Repository: https://github.com/<your-username>/<repo-name>
# License: MIT
#
# Version: 1.0.0
#
# Change Log:
# ------------------------------------------------------------------------------
# 2025-05-09 (v1.0.0) - Initial release
#   * Created `test_tbc_data()` for simulating respondent-level data
#   * Created `tbc_choice_sets()` for converting responses into long-format sets
#   * Added documentation, example usage, and progress bar support
#
# Usage:
#   source("tbc_utils.R")
#   
#   # Simulate data for 10 respondents completing 3 tasks
#   df <- test_tbc_data(
#     n = 10,
#     number_of_tasks = 3,
#     total_items = 10,
#     number_of_items_shown = 4,
#     uid_colname = "sys_RespNum",
#     first_response_prefix = "TBC",
#     dual_response_prefix = "TBCDR"
#   )
#   
#   # Generate choice sets suitable for modeling
#   choice_sets <- tbc_choice_sets(
#     df = df,
#     number_of_tasks = 3,
#     total_items = 10,
#     number_of_items_shown = 4,
#     uid_colname = "sys_RespNum",
#     first_response_prefix = "TBC",
#     dual_response_prefix = "TBCDR",
#     allocation = c(0, 0, 0, 0.3, 0.7)
#   )
#
# Contact:
#   For questions or contributions, please reach out via GitHub issues or 
#   contact Megan Peitz <megan@numerious.com>.
# ==============================================================================







#' Test Token-Based Conjoint (TBC) Response Data Structure
#'
#' This function generates synthetic respondent-level data suitable for 
#' use with the `tbc_choice_sets()` function. Each respondent is randomly 
#' assigned a series of TBC tasks, with both forced-choice and dual-response 
#' selections recorded using a consistent column naming convention.
#'
#' @param n Integer. Number of simulated respondents.
#' @param number_of_tasks Integer. Number of TBC tasks each respondent will complete.
#' @param total_items Integer. Total number of unique items that may appear across tasks.
#' @param number_of_items_shown Integer. Number of items shown per task.
#' @param uid_colname String. Name of the column to store respondent IDs (e.g., `"sys_RespNum"`).
#' @param first_response_prefix String. Prefix used for the forced-choice response columns.
#' These are named using the pattern `<prefix>_<item index>.<task number>`.
#' @param dual_response_prefix String. Prefix used for the dual-response (none vs. chosen set) columns.
#' These are named using the pattern `<prefix>.<task number>`.
#'
#' @return A data frame with `n` rows and columns structured for input to `tbc_choice_sets()`. 
#' Includes:
#' - A respondent ID column (named per `uid_colname`)
#' - Forced choice responses for each item-task pair (with 0/1 or NA values)
#' - Dual-response codes for each task (random integers from 1 to 5)
#'
#' @examples
#' df <- test_tbc_data(
#'   n = 10,
#'   number_of_tasks = 3,
#'   total_items = 10,
#'   number_of_items_shown = 4,
#'   uid_colname = "sys_RespNum",
#'   first_response_prefix = "TBC",
#'   dual_response_prefix = "TBCDR"
#' )
#'
#' choice_sets <- tbc_choice_sets(
#'   df = df,
#'   number_of_tasks = 3,
#'   total_items = 10,
#'   number_of_items_shown = 4,
#'   uid_colname = "sys_RespNum",
#'   first_response_prefix = "TBC",
#'   dual_response_prefix = "TBCDR"
#' )
#'
#' @export
test_tbc_data <- function(n, number_of_tasks, total_items, number_of_items_shown,
                          uid_colname="sys_RespNum", 
                          first_response_prefix="TBC",
                          dual_response_prefix="TBCDR"){
  
  stopifnot(n > 0, number_of_tasks > 0, total_items >= number_of_items_shown)
  
  all_fr_names <- c()
  for(task_i in 1:number_of_tasks){
    all_fr_names <- c(all_fr_names,  paste0(first_response_prefix,"_", 1:total_items, ".", task_i))
  }
  all_dr_names <- paste0(dual_response_prefix,".", 1:number_of_tasks)
  
  width <- length(all_fr_names) + length(all_dr_names) + 1
  out <- matrix(NA,nrow=n,ncol=width) |> as.data.frame()
  colnames(out) <- c(uid_colname,all_fr_names,all_dr_names)
  
  
  for(i in seq_len(n)){
    out[i, uid_colname] <- i
    all_k <- sample(1:(number_of_items_shown-1), number_of_tasks, replace = T)
    
    for(task_j in seq_len(number_of_tasks)){
      shown_items <- sample(1:total_items, number_of_items_shown)
      choosen_index <- sample(1:number_of_items_shown, all_k[task_j])
      not_choosen_index <- setdiff(1:number_of_items_shown, choosen_index)
      for(index_i in choosen_index){
        item_i <- shown_items[index_i]
        out[i, paste0(first_response_prefix,"_", item_i, ".", task_j)] <- 1
      }
      for(index_i in not_choosen_index){
        item_i <- shown_items[index_i]
        out[i, paste0(first_response_prefix,"_", item_i, ".", task_j)] <- 0
      }
      out[i, all_dr_names[task_j]] <- sample(1:5,1)
    }
  }
  return(out)
  
}




#' Generate Token-Based Conjoint (TBC) Choice Sets from Response Data
#'
#' This function creates a long-format data frame of choice sets for a 
#' Token-Based Conjoint (TBC) design based on individual-level responses.
#' Each respondent’s task is represented by two parts:
#' - A forced choice set without a "none" option.
#' - A dual-response set including a "none" option, with response weights 
#'   determined by the provided `allocation` vector.
#'
#' @param df A data frame containing individual respondent-level data, 
#' including forced and dual-response answers for each TBC task.
#' @param number_of_tasks Integer specifying the total number of TBC tasks shown per respondent.
#' @param total_items Integer total number of unique items (e.g., brands, products) used in the conjoint.
#' @param number_of_items_shown Integer number of items shown in each choice task.
#' @param uid_colname String. The name of the column in `df` that identifies each respondent uniquely.
#' @param first_response_prefix Prefix used for the forced-choice responses.
#' Columns should follow the pattern: `<prefix>_<item index>.<task number>`, 
#' e.g., `TBC_1.3` for item 1 in task 3.
#' @param dual_response_prefix Prefix for the dual-response ("none" vs. shown set) column.
#' Columns should follow the pattern: `<prefix>.<task number>`, e.g., `TBCDR.3`.
#' @param allocation A numeric vector mapping dual response codes (integers) 
#' to the probability of choosing the "none" option. For example, `allocation = c(0, 0, 0, 0.3, 0.7)`
#' means code 4 maps to 30% none, and code 5 maps to 70% none.
#' @param exclude_task Optional integer vector indicating task numbers to skip when generating choice sets.
#'
#' @return A long-format data frame with one row per alternative per choice set. 
#' Each row includes:
#' - `ID`: respondent ID
#' - `Set`: task number
#' - `Position`: row position within the choice set
#' - `b1, b2, ..., bN`: binary indicators for item presence
#' - `none`: binary indicator if the row corresponds to the "none" option
#' - `Response`: 1 if selected, 0 otherwise
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- tbc_choice_sets(
#'   df = my_data,
#'   number_of_tasks = 8,
#'   total_items = 6,
#'   number_of_items_shown = 2,
#'   uid_colname = "RespondentID",
#'   first_response_prefix = "TBC",
#'   dual_response_prefix = "TBCDR",
#'   allocation = c(0, 0, 0, 0.3, 0.7),
#'   exclude_task = c(3, 6)
#' )
#' }
#'
#' @export

tbc_choice_sets <- function(df, number_of_tasks, total_items, number_of_items_shown,
                       uid_colname="sys_RespNum", 
                       first_response_prefix="TBC",
                       dual_response_prefix="TBCDR",
                       allocation=c(0,0,0,.3,.7), 
                       exclude_task = c()) {
  
  stopifnot(number_of_tasks > 0, total_items >= number_of_items_shown)
  
  
  tbc_task_with_none <- function(total_items, id, set_i, combos, dr, allocation) {
    pnames <- c("ID", "Set", "Position", paste0("b", 1:total_items), "none", "Response")
    choice_set <- matrix(0, ncol = length(pnames), nrow = 2) |> as.data.frame()
    colnames(choice_set) <- pnames
    choice_set$ID <- id
    choice_set$Set <- set_i
    choice_set$Position <- 1:nrow(choice_set)

    if (dr < 1 || dr > length(allocation)) {
      stop(paste("Invalid dual response code:", dr))
    }
    choice_set$Response <- c(1 - allocation[dr], allocation[dr])
    choice_set$none <- c(1, 0)
    choice_set[1, paste0("b", 1:total_items)] <- 0
    for (i in colnames(combos)) {
      choice_set[[i]][2] <- combos[[i]][1]
    }
    return(choice_set)
  }
  
  tbc_task_without_none <- function(total_items, id, set_i, combos) {
    pnames <- c("ID", "Set", "Position", paste0("b", 1:total_items), "none", "Response")
    choice_set <- matrix(0, ncol = length(pnames), nrow = nrow(combos)) |> as.data.frame()
    colnames(choice_set) <- pnames
    choice_set$ID <- id
    choice_set$Set <- set_i
    choice_set$Position <- 1:nrow(choice_set)
    choice_set$Response <- c(1, rep(0, nrow(choice_set) - 1))
    choice_set$none <- 0
    for (i in colnames(combos)) {
      choice_set[[i]] <- combos[[i]]
    }
    
    return(choice_set)
  }
  
  
  allocation <- as.double(allocation)

  args <- list()
  for(i in 1:number_of_items_shown){
    args[[paste0("i",i)]] <- 0:1
  }
  args[["stringsAsFactors"]] <- F
  
  defcombs <- do.call(expand.grid,args)
  defcounts <- apply(defcombs, 1, function(x) {
    sum(x == 1)
  })
  ptasks <- setdiff(1:number_of_tasks, exclude_task)
  output <- vector("list", nrow(df)*length(ptasks)*2)
  output_i <- 1
  pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
  for (i in seq_len(nrow(df))) {
    uid <- df[[uid_colname]][i]
    choice_set_i <- 1
    for (j in ptasks) {
      fr <- df[i, paste0(first_response_prefix, "_", 1:total_items, ".", j)]
      dr <- df[i, paste0(dual_response_prefix,".", j)]
      win <- which(fr %in% 1)
      lose <- which(fr %in% 0)
      combos <- defcombs[which(defcounts %in% length(win)), ]
      colnames(combos) <- paste0("b", c(win, lose))
      for (ai in rev(paste0("b", c(win, lose)))) {
        combos <- combos[order(-combos[[ai]]), ]
      }
      output[[output_i]] <- tbc_task_without_none(total_items, id = uid, set_i = choice_set_i, combos)
      output_i <- output_i + 1
      choice_set_i <- choice_set_i + 1
      
      output[[output_i]] <- tbc_task_with_none(total_items, id = uid, set_i = choice_set_i, combos, dr,  allocation)
      output_i <- output_i + 1
      choice_set_i <- choice_set_i + 1
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  output_df <- dplyr::bind_rows(output)
  return(output_df)
}


