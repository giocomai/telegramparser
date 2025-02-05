#' Set the channel username for locally stored Telegram channels
#'
#' Channel username is necessary to re-create links to channels and posts.
#' Unfortunately, it cannot be retrieved from the export json files themselves
#' and needs to be added manually.
#'
#' Information is cached locally in a file called `tp_usernames.csv` located
#' in the path folder.
#'
#' If no `channel_username` is given, the user will be requested to add it
#' interactively.
#'
#' @param channel_username Username of the Telegram channel, either as link
#'   (e.g. "https://t.me/username"), as username ("@username"), or as plain text
#'   ("username"). Either way, the username is stored consistently internally as
#'   plain text.
#' @inheritParams tp_get_username
#'
#' @returns A data frame with one row and three columns (`channel_id`,
#'   `channel_name`, `channel_username`) if relevant data has previously been
#'   stored. Otherwise, returns NULL.
#' @export
#'
#' @examples
#' tp_set_username(
#'   channel_name = "BBC News | Русская служба",
#'   channel_username = "bbcrussian"
#' )
tp_set_username <- function(channel_name = NULL,
                            channel_id = NULL,
                            channel_username = NULL,
                            path = NULL) {
  path <- tp_get_options(path = path)[["path"]]

  if (is.null(channel_id)) {
    current_file <- tp_select(
      path = path,
      channel_name = channel_name
    )
    if (is.null(current_file)) {
      cli::cli_abort(
        c(
          x = "No local archive for {.val {channel_name}} found in {.path {path}}",
          i = "You can still store the channel username by providing both {.var channel_name} and {.var channel_id}, or set the path to a folder where the relevant Telgram archive is stored."
        )
      )
    }
    channel_id <- current_file |>
      tp_get_metadata() |>
      dplyr::pull(channel_id)
  } else if (is.null(channel_name)) {
    current_file <- tp_select(
      path = path,
      channel_id = channel_id
    )
    if (is.null(current_file)) {
      cli::cli_abort(
        c(
          x = "No local archive for {.val {channel_id}} found in {.path {path}}",
          i = "You can still store the channel username by providing both {.var channel_name} and {.var channel_id}, or set the path to a folder where the relevant Telgram archive is stored."
        )
      )
    }
    channel_name <- tp_get_metadata() |>
      dplyr::pull(channel_name)
  } else {
    cli::cli_abort("Either {.var channel_id} or {.var channel_name} must be given.")
  }


  previous_df <- tp_get_username(
    channel_id = channel_id,
    channel_name = channel_name,
    path = path
  )

  if (!is.null(previous_df)) {
    if (is.null(channel_username) == FALSE) {
      if (previous_df[[channel_username]] != channel_username) {
        cli::cli_alert_warning(
          "Given {.val {channel_username}} does not match previously stored
        channel username {.val {previous_df[[channel_username]]}} for Telegram
        channel with name {.val {channel_name}} and id {.val {channel_id}}.
          Please fix this manually." |>
            stringr::str_squish()
        )
      }
    }
  }

  if (is.null(channel_username)) {
    channel_username <- readline(
      prompt = cli::cli_text("Insert channel username for channel with name {.val {channel_name}} and id {.val {channel_id}}: ")
    )
  }

  channel_username <- channel_username |>
    stringr::str_remove(pattern = stringr::regex("^https://t.me/")) |>
    stringr::str_remove(pattern = stringr::regex("^t.me/")) |>
    stringr::str_remove(pattern = stringr::regex("^@"))

  channel_df <- tibble::tibble(
    channel_id = channel_id,
    channel_name = channel_name,
    channel_username = channel_username
  )

  if (fs::file_exists(fs::path(path, "tp_usernames.csv"))) {
    readr::write_csv(
      x = channel_df,
      file = fs::path(path, "tp_usernames.csv"),
      append = TRUE
    )
  } else {
    readr::write_csv(
      x = channel_df,
      file = fs::path(path, "tp_usernames.csv"),
      append = FALSE
    )
  }

  channel_df
}


#' Checks if a channel username has been stored locally
#'
#' Channel usernames are necessary to reproduce urls to posts.
#'
#' @param path Path to a folder where a `tp_usernames.csv` file is expected to
#'   be located. This will usually be the same folder where Telegram archives
#'   are stored.
#'
#' @returns A data frame with one row and three columns (`channel_id`,
#'   `channel_name`, `channel_username`) if relevant data has previously been
#'   stored. Otherwise, returns NULL.
#' @export
#'
#' @examples
#' tp_set_username(channel_name = "BBC News | Русская служба")
tp_get_username <- function(channel_name = NULL,
                            channel_id = NULL,
                            path = NULL) {
  path <- tp_get_options(path = path)[["path"]]

  if (fs::file_exists(fs::path(path, "tp_usernames.csv"))) {
    all_channels_df <- readr::read_csv(
      file = fs::path(path, "tp_usernames.csv"),
      col_types = "icc"
    )

    if (!is.null(channel_id) & is.null(channel_name)) {
      channel_df <- all_channels_df |>
        dplyr::filter(
          .data[["channel_id"]] %in% .env[["channel_id"]],
          .data[["channel_name"]] %in% .env[["channel_name"]]
        )
      return(channel_df)
    } else if (!is.null(channel_id)) {
      channel_df <- all_channels_df |>
        dplyr::filter(
          .data[["channel_id"]] %in% .env[["channel_id"]]
        )
      return(channel_df)
    } else if (!is.null(channel_name)) {
      channel_df <- all_channels_df |>
        dplyr::filter(
          .data[["channel_name"]] %in% .env[["channel_name"]]
        )
      return(channel_df)
    }
  } else {
    return(NULL)
  }
}


#' Interactively set all Telegram channel usernames not yet stored locally
#'
#' @inheritParams tp_get_username
#'
#' @returns
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tp_set_all_usernames()
#' }
tp_set_all_usernames <- function(path) {
  path <- tp_get_options(path = path)[["path"]]

  metadata_df <- tp_get_metadata(path = path) |>
    dplyr::distinct(channel_id, .keep_all = TRUE)

  channels_df <- readr::read_csv(
    file = fs::path(path, "tp_usernames.csv"),
    col_types = "icc"
  )

  missing_usernames_df <- metadata_df |>
    dplyr::anti_join(
      y = channels_df,
      by = "channel_id"
    )

  purrr::walk(
    .x = purrr::transpose(missing_usernames_df),
    .f = \(current_channel) {
      tp_set_username(
        channel_name = current_channel[["channel_name"]],
        channel_id = current_channel[["channel_id"]],
        path = path
      )
    }
  )

  channels_df <- readr::read_csv(
    file = fs::path(path, "tp_usernames.csv"),
    col_types = "icc"
  )

  channels_df
}
