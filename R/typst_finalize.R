setMethod(
  f = "finalize",
  signature = "tinytable_typst",
  definition = function(x, ...) {
    out <- x@table_string

    cap <- x@caption
    if (length(cap) == 1) {
      out <- sub(
        "$TINYTABLE_TYPST_CAPTION",
        sprintf("caption: %s,", cap),
        out,
        fixed = TRUE
      )
    } else {
      out <- sub("$TINYTABLE_TYPST_CAPTION", "", out, fixed = TRUE)
    }

    # drop the full header block if there are no colnames or group headers
    if (length(x@names) == 0 && nrow(x@group_data_j) == 0) {
      out <- lines_drop_between(
        out,
        regex_start = "// tinytable header start",
        regex_end = "// tinytable header end",
        fixed = TRUE
      )
    }

    # Quarto wraps the table in its own figure when the chunk carries a `tbl-`
    # prefixed label or a `tbl-cap` option. Only then do we drop ours, otherwise
    # the two nest and the caption set by `tt(caption = )` is thrown away.
    quarto_figure <- FALSE
    if (isTRUE(check_dependency("knitr")) && isTRUE(knitr::pandoc_to("typst"))) {
      lab <- knitr::opts_current$get()[["label"]]
      cap <- knitr::opts_current$get()[["tbl-cap"]]
      quarto_figure <- (!is.null(lab) && grepl("^tbl-", lab)) || !is.null(cap)
    }

    if (quarto_figure) {
      # Remove figure environment from template and let Quarto use its own
      out <- lines_drop_between(
        out,
        regex_start = "// start preamble figure",
        regex_end = "// end preamble figure",
        fixed = TRUE
      )
      out <- lines_drop(out, regex = "// end figure", fixed = TRUE)
      out <- sub(" table(", " #table(", out, fixed = TRUE)
    } else {
      # here we kept tinytable's #figure[] so we need to use block[]
      out <- sub("#block", "block", out, fixed = TRUE)
    }

    x@table_string <- out

    return(x)
  })
