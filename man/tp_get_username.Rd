% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tp_set_username.R
\name{tp_get_username}
\alias{tp_get_username}
\title{Checks if a channel username has been stored locally}
\usage{
tp_get_username(channel_name = NULL, channel_id = NULL, path = NULL)
}
\arguments{
\item{path}{Path to a folder where a \code{tp_usernames.csv} file is expected to
be located. This will usually be the same folder where Telegram archives
are stored.}
}
\value{
A data frame with one row and three columns (\code{channel_id},
\code{channel_name}, \code{channel_username}) if relevant data has previously been
stored. Otherwise, returns NULL.
}
\description{
Channel usernames are necessary to reproduce urls to posts.
}
\examples{
tp_set_username(channel_name = "BBC News | Русская служба")
tp_get_username(channel_name = "BBC News | Русская служба")
}
