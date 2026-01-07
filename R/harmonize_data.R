#' Harmonizes inconsistent labels or entities by mapping aliases to a target value.
#' Useful for cleaning up country names (e.g., mapping "Belgium", "Luxembourg", "Netherlands" -> "Benelux")
#' or grouping entities into broader categories.
#'
#' @param df Data frame containing at least the column specified in `along`.
#' @param maps A named list where names are the target labels and values are character vectors 
#'        of aliases or patterns to match (e.g., `list("Benelux" = c("Belgium", "Luxembourg", "Netherlands"))`).
#' @param along Name of the column to search for matches (e.g., "label", "word", "clean_label"). Default: "label".
#' @param regex Logical; if TRUE, aliases in `maps` are treated as regular expressions.
#' @param case_insensitive Logical; if TRUE, matching ignores case differences.
#' @param out_col Name of the output column to store the harmonized labels. Default: "new_label". If set to the same name as an existing column, it will be overwritten.
#' @param conflict Strategy to resolve multiple matches per row: 
#'        "last_wins" (the last match in `maps` overwrites previous ones) or 
#'        "first_wins" (the first match is kept).
#' 
#' @return A data frame with the added or updated column `out_col`. 
#'         Values not matched in `maps` retain their original value from the `label` column 
#'         (or `along` column if used as fallback).
#' 
#' @import dplyr tidyr lubridate rlang stringr
#' @importFrom stats setNames
#' @export
harmonize_data <- function(df,
                           maps = list(),
                           along = "label",
                           regex = FALSE,
                           case_insensitive = TRUE,
                           out_col = "new_label",
                           conflict = c("last_wins","first_wins")) {
  
  # Validation
  conflict <- match.arg(conflict)
  validate_columns(df, required = along)
  
  df[[out_col]] <- target_vec <- df[[along]]
  if (!length(maps)) { return(df) }
    
  already_set <- rep(FALSE, nrow(df))
  
  if (!regex) {
    # Exact match path (fast)
    if (case_insensitive) {
      target_vec_fold <- tolower(target_vec)
    } else {
      target_vec_fold <- target_vec
    }
    all_aliases <- unlist(maps, use.names = FALSE)
    all_targets <- rep(names(maps), lengths(maps))
    if (case_insensitive) all_aliases <- tolower(all_aliases)
    
    lookup <- setNames(all_targets, all_aliases)
    if (conflict == "first_wins") {
      # Remove duplicates from lookup, keeping the first occurrence
      keep <- !duplicated(all_aliases)
      lookup <- setNames(all_targets[keep], all_aliases[keep])
    }
    
    hit <- match(target_vec_fold, names(lookup), nomatch = 0L)
    if (any(hit > 0L)) {
      idx <- which(hit > 0L)
      if (conflict == "first_wins") idx <- idx[!already_set[idx]] # Only overwrite if not already set
      df[[out_col]][idx] <- lookup[ hit[idx] ]
      already_set[idx] <- TRUE
    }
  } else {
    # Regex path
    for (tgt in names(maps)) {
      pats <- maps[[tgt]]
      if (!length(pats)) next
      pat <- paste(pats, collapse = "|")
      if (case_insensitive) pat <- paste0("(?i)", pat)
      idx <- grepl(pat, target_vec, perl = TRUE)
      if (any(idx)) {
        if (conflict == "first_wins") idx <- idx & !already_set
        df[[out_col]][idx] <- tgt
        already_set[idx] <- TRUE
      }
    }
  }
  df
}
