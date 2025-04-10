% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tp_read_archive.R
\name{tp_read_archive}
\alias{tp_read_archive}
\title{Read archives created with \code{tg-archive}}
\usage{
tp_read_archive(path, type = c("messages", "users", "media"))
}
\arguments{
\item{path}{Path to the \code{sqlite} file or folder containing it.}

\item{type}{Defaults to "messages". Valid options include "messages",
"users", and "media".}
}
\value{
A data frame with stored messages, users, or media, depending on
type.
}
\description{
See https://github.com/knadh/tg-archive/
}
\examples{
if (interactive) {
 tp_read_archive()
}
}
