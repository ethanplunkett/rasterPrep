.extract.tag <- function(tag, x, numeric = FALSE) {
  # This helper function extracts the non blank text after the tag continuing
  # to next blank.
  # It throws an error if the tag occurs more than once
  # If numeric is TRUE it converts the result to a number
  ln <- grep(tag, x, ignore.case = TRUE)
  if (length(ln) > 1)
    stop("More than one", tag, "tag found.")
  if (length(ln) == 0)
    return(NA)
  res <- gsub(paste0("(^.*", tag, "[[:blank:]]*)([^[:blank:]]*)(.*$)"), "\\2",
              x[ln], ignore.case = TRUE)

  # Drop training , as when it's there we generally don't want it
  res <- gsub(",$", "", res)
  if (numeric) {
    # These are cases that have shown up sometimes
    # and what I think they are supposed to map to.
    res <- switch(res,
                  "1.#IO" = Inf,
                  "-1.#IO" = -Inf,
                  "1.#QO" = NA,
                  "nan" = NA,
                  "-inf" = -Inf,
                  "inf" = Inf,
                  res)

    if (is.character(res) &&
          grepl("[^[:digit:]\\.+e-]", res, ignore.case = TRUE))
      stop("Looking up numeric tag", tag, "but found non numeric result:", res)
    res <- as.numeric(res)
  }
  return(res)
}
