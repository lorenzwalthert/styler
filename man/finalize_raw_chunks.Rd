% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform-code.R
\name{finalize_raw_chunks}
\alias{finalize_raw_chunks}
\title{Drop start / stop, when formatting is turned off}
\usage{
finalize_raw_chunks(start, end, filetype, lines)
}
\description{
If \code{tidy = FALSE} (the knitr code chunk default), code is not styled upon
knitting. If it is explicitly added to a code chunk, the code chunk is in
addition not styled with styler when formatting the document.
}
\keyword{internal}
