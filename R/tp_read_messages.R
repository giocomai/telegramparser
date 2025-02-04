#' Transform a full import of Telegram into a tibble with textual contents as plain character strings
#'
#' A unique `doc_id` column is added in order to provide a unique identifier when combinining different channels.
#'
#' @param telegram_df A data frame, typically created with [tp_read_json()]
#'
#' @returns A tibble; none of the columns is a list.
#' @export
#'
#' @examples
tp_read_messages <- function(telegram_df) {
  telegram_df |>
    dplyr::filter(type == "message") |>
    dplyr::mutate(text = purrr::map_chr(
      .x = text_entities,
      .f = function(x) {
        if (length(x) == 0) {
          NA_character_
        } else if (nrow(x) == 0) {
          NA_character_
        } else {
          x[["text"]] |>
            stringr::str_flatten(collapse = " ", na.rm = TRUE)
        }
      }
    )) |>
    dplyr::mutate(
      datetime = lubridate::as_datetime(as.numeric(date_unixtime))
    ) |>
    dplyr::mutate(date = lubridate::as_date(datetime)) |>
    tidyr::unite(
      col = "doc_id",
      channel_name,
      id,
      sep = "_",
      remove = FALSE,
      na.rm = FALSE
    ) |>
    dplyr::select(
      doc_id,
      text,
      date,
      datetime,
      id,
      reply_to_message_id,
      channel_name,
      channel_id
    )
}
