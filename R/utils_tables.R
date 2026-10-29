# ---------------------------------------------------------------------------
# One table style for the whole app.
#
# Every DT in the app goes through sl_table(): same class, same footer, no
# per-column filter boxes, numeric columns right aligned and rounded once.
# ---------------------------------------------------------------------------

DT_LANGUAGE <- list(
  search           = "",
  searchPlaceholder = "Filter rows",
  info             = "_START_-_END_ of _TOTAL_",
  infoEmpty        = "no rows",
  infoFiltered     = "(filtered from _MAX_)",
  emptyTable       = "Nothing to show",
  zeroRecords      = "No row matches the filter",
  paginate         = list(previous = "Prev", `next` = "Next")
)

#' Shared DT constructor
#'
#' @param data Data frame to render.
#' @param page_length Rows per page; `NULL` shows every row with no pager.
#' @param searchable Adds the single search box above the table.
#' @param digits Significant digits for the numeric columns, or `NULL` to leave
#'   the values untouched (already formatted upstream).
#' @param plain Drops the bold treatment of the first column.
#' @param scroll_x Kept for call-site clarity; horizontal overflow is handled
#'   by the wrapper, see www/styles.css.
sl_table <- function(data, page_length = 10, searchable = FALSE, digits = 4,
                     plain = FALSE, scroll_x = TRUE, ordering = TRUE,
                     round_cols = NULL, signif_cols = NULL, colnames = NULL,
                     no_header = FALSE, wrap = FALSE) {
  data <- as.data.frame(data)
  paged <- !is.null(page_length) && nrow(data) > page_length

  dom <- if (paged) "tip" else "t"
  if (searchable) dom <- paste0("f", dom)

  # Numeric columns are right aligned, header included, so the digits and the
  # label line up on the same edge.
  # Identifiers are numbers but read as labels: keep them on the left.
  ID_COLS  <- c("Month", "Lag", "lag", "Year", "Day", "n", "N")
  is_num   <- vapply(data, is.numeric, logical(1)) & !(names(data) %in% ID_COLS)
  num_cols <- names(data)[is_num]
  txt_cols <- names(data)[!is_num]
  # unname(): a named vector reaches DataTables as a JSON object and the
  # targets are silently ignored.
  col_defs <- list()
  if (length(num_cols) > 0) {
    col_defs <- c(col_defs, list(list(className = "dt-right",
                                      targets = unname(which(is_num) - 1L))))
  }
  if (length(txt_cols) > 0) {
    col_defs <- c(col_defs, list(list(className = "dt-left dt-head-left",
                                      targets = unname(which(!is_num) - 1L))))
  }

  cls <- paste("solar-table stripe hover compact",
               if (plain) "sl-plain" else "",
               if (isTRUE(no_header)) "sl-noheader" else "",
               if (isTRUE(wrap)) "sl-wrap" else "")

  args <- list(
    data      = data,
    rownames  = FALSE,
    selection = "none",
    class     = trimws(cls),
    escape    = TRUE,
    options   = list(
      dom          = dom,
      pageLength   = if (is.null(page_length)) nrow(data) else page_length,
      lengthChange = FALSE,
      # DataTables' own scrollX splits the table into a header table and a body
      # table, which leaves a seam under the header and can drift out of
      # alignment. The wrapper scrolls instead, keeping the two together.
      scrollX      = FALSE,
      ordering     = ordering,
      autoWidth    = FALSE,
      columnDefs   = col_defs,
      language     = DT_LANGUAGE
    )
  )
  if (!is.null(colnames)) args$colnames <- colnames

  args$class <- trimws(gsub("\\s+", " ", args$class))
  tab <- do.call(DT::datatable, args)

  # Rounding: explicit column lists win, otherwise every numeric column that is
  # not an integer identifier gets the same number of significant digits.
  if (!is.null(round_cols)) {
    tab <- DT::formatRound(tab, intersect(round_cols, names(data)), digits = digits)
  }
  if (!is.null(signif_cols)) {
    tab <- DT::formatSignif(tab, intersect(signif_cols, names(data)), digits = digits)
  }
  if (is.null(round_cols) && is.null(signif_cols) && !is.null(digits)) {
    auto <- num_cols[vapply(data[num_cols], function(x) !is.integer(x), logical(1))]
    auto <- setdiff(auto, c("Month", "n", "lag", "nobs", "N", "Year", "Day"))
    if (length(auto) > 0) tab <- DT::formatSignif(tab, auto, digits = digits)
  }
  tab
}

#' Colour a pass/fail column
sl_outcome <- function(tab, column, pass_values) {
  DT::formatStyle(
    tab, column,
    backgroundColor = DT::styleEqual(pass_values,
                                     rep("var(--sl-ok-soft)", length(pass_values)),
                                     default = "var(--sl-bad-soft)"),
    color = DT::styleEqual(pass_values,
                           rep("var(--sl-ok)", length(pass_values)),
                           default = "var(--sl-bad)"),
    fontWeight = "600"
  )
}

#' Two-column key/value table used for the summary panels
sl_kv_table <- function(data) {
  sl_table(data, page_length = NULL, digits = NULL, scroll_x = FALSE,
           ordering = FALSE, colnames = c("", ""), no_header = TRUE, wrap = TRUE)
}

#' Placeholder table carrying a message instead of data
sl_message_table <- function(msg) {
  sl_table(data.frame(Message = as.character(msg)), page_length = NULL,
           digits = NULL, plain = TRUE, scroll_x = FALSE, ordering = FALSE,
           colnames = "", no_header = TRUE, wrap = TRUE)
}
