#' @title Remove unwanted cells from xlsx_cells data
#'
#' @description Remove cells using specified cell addresses and cells containing
#' strings where the font is the same colour as the background.
#'
#' @details
#' The cells removed by this function are usually specified as needing removal
#' because not doing so results in errors during beheading (un-pivotting).
#'
#' For more information and for examples, see the documentation for
#' remove_unwanted_cells and remove_hidden_character_strings.
#'
#' @param dat dataframe that has been imported using xlsx_cells.
#' @param cells_to_remove character string.
#' @param input_filepath character string.
#' @param hidden_strings bool. TRUE, FALSE or NA. Default is NA. FALSE leads to
#' the same behaviour as NA. In pub sec this variable is specified by
#' remove_hidden_strings_bool.
#'
#' @returns dataframe (dat) with unwanted information removed
remove_from_input <- function(
    dat, cells_to_remove = NA, input_filepath = NA, hidden_strings = NA
) {

  unwanted_cells_removed <- remove_unwanted_cells(dat, cells_to_remove)

  cells_removed <- remove_hidden_character_strings(
    unwanted_cells_removed, input_filepath, hidden_strings
  )

  return(cells_removed)
}


#' @title Remove cells from input data that are not needed.
#'
#' @description Remove rows from an xlsx_cells imported dataframe
#' using cell addresses to identify which rows to remove.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells
#' @param cells vector of character strings. The Excel addresses of
#' cells to be removed from data e.g. 'A1'. In pub sec this variable is
#' specified by cells_to_remove
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(address = c("A1", "A2", "A3"), numeric = 1:3)
#'
#' cells_to_remove <- c("A1", "A2")
#'
#' remove_unwanted_cells(dat, cells_to_remove)
#' }
#' @export
remove_unwanted_cells <- function(dat, cells = NA) {

  if (all(is.na(cells))) {return(dat)}

  message ("Removing cells specified by address.")

  filtered <- dat
  for (i in 1:length(cells)) {

    cell <- cells[i]

    if (! cell %in% dat$address) {
      warning(
        "Cell ", cell, " has not been found in the data, so cannot be removed."
      )
    } else {

      type <- dat$data_type[dat$address == cell]
      content <- dat[[type]][dat$address == cell]
      removed_content <- ifelse(
        is.null(content), "nothing", paste0("'", content, "'")
      )

      filtered <- filter(filtered, address != cell)

      warning(
        "Cell ", cell, " containing ", removed_content, " has been removed."
        )
    }
  }

  return(filtered)

}


#' @title Remove hidden character strings in the first header row.
#'
#' @description Remove cells whose text is the same colour as the background.
#' This function was created because in pub sec scot gov HRA 2021-22 there are
#' hidden character strings in the
#' first header row that are hidden by being the same colour as the background.
#' These need to be removed so that un-pivotting works correctly.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells()
#' @param filepath string. Filepath for input data to detect cell formats.
#' @param remove_cell bool. TRUE, FALSE or NA. Default is NA. FALSE leads to the
#' same behaviour as NA. In pub sec this variable is specified by
#' remove_hidden_strings_bool.
#'
#' @returns dataframe. dat with rows removed for the identified cells. A warning
#' is raised if rows are removed.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(address = c("A1"),
#'                   local_format_id = c(1))
#' filepath <- D:/.../interim/test_dat.xlsx
#' remove_hidden_strings_bool <- "TRUE"
#'
#' remove_hidden_character_strings(dat, filepath, remove_hidden_strings_bool)
#' }
#' @export
remove_hidden_character_strings <- function(dat, filepath, remove_cell=NA) {

  if (is.na(remove_cell)) {
    return(dat)
  } else if (remove_cell == FALSE) {
    return(dat)
  }

  message(
    "Removing cells where the text is the same colour as the background ",
    "(hidden info)."
  )

  # get all the formatting info from excel
  formats <- xlsx_formats(filepath)
  # we are only interested in the colour 'codes' for this situation
  tint <- formats$local$font$color$tint
  rgb <- formats$local$font$color$rgb
  white_text_codes <- which(rgb=="FFFFFFFF" & is.na(tint))

  hidden_characters_to_remove <- dat %>%
    filter(local_format_id %in% white_text_codes) %>%
    select(address) %>% pull()

  if (length(hidden_characters_to_remove) > 0) {
    dat <- dat %>%
      filter(local_format_id %in% white_text_codes == FALSE)

    warning(
      "The following cells have been removed as they have been flagged ",
      "as having hidden and text that is not required: ",
      paste(hidden_characters_to_remove, collapse = ", "), "."
    )
  }
  return(dat)
}

