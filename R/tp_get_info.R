#' Retrieve from the web basic information about the Telegram channel
#'
#' Information retrieved includes channel title, description, number of
#' followers, and url to (cached) image.
#'
#' @inheritParams tp_get_username
#'
#' @returns A tibble.
#' @export
#'
#' @examples
tp_get_info <- function(channel_id = NULL,
                        channel_name = NULL) {
  base_info_df <- tp_get_username(
    channel_id = channel_id,
    channel_name = channel_name
  )

  info_df <- purrr::pmap(
    .l = list(
      base_info_df[["channel_id"]],
      base_info_df[["channel_username"]],
      base_info_df[["channel_name"]]
    ),
    .f = \(
      current_channel_id,
      current_channel_username,
      current_channel_name) {
      html <- rvest::read_html(x = stringr::str_flatten(
        string = c(
          "https://t.me/",
          current_channel_username
        ),
        collapse = ""
      ))

      tibble::tibble(
        channel_id = current_channel_id,
        channel_username = current_channel_username,
        channel_name = current_channel_name,
        title = html |>
          rvest::html_elements(xpath = "//div[@class='tgme_page_title']") |>
          rvest::html_text2(),
        description = html |>
          rvest::html_elements(xpath = "//div[@class='tgme_page_description']") |>
          rvest::html_text2(),
        subscribers = html |>
          rvest::html_elements(xpath = "//div[@class='tgme_page_extra']") |>
          rvest::html_text2() |>
          stringr::str_extract_all(pattern = "[[:digit:]]+", simplify = TRUE) |>
          stringr::str_flatten() |>
          as.numeric(),
        image_url = html |>
          rvest::html_elements(xpath = "//img[@class='tgme_page_photo_image']") |>
          xml2::xml_attr("src"),
        retrieved_at = Sys.time()
      )
    }
  ) |>
    purrr::list_rbind()
  info_df
}
