library(tidyverse)
library(gt)

sectors <- read_csv("data-raw/sectors.csv")

fastest_split_by_run <- function(.data, section) {
  .data |>
    slice_min({{ section }}, na_rm = TRUE) |>
    pull(round_type) |>
    as.character()
}

highlight_fastest_splits <- function(gt_tbl, data) {
  reduce(
    .x = 1:5,
    .f = \(gt, i) {
      col_sym <- rlang::sym(as.character(i))
      gt |>
        data_color(
          columns = !!col_sym,
          rows = round_type == fastest_split_by_run(data, !!col_sym),
          palette = "#4daf4a"
        )
    },
    .init = gt_tbl
  )
}

fastest_splits_gt <- function(name, event_name) {
  tbl <- sectors |>
    filter(name == {{ name }}) |>
    filter(event_name == {{ event_name }}) |>
    select(-name, -event_name) |>
    mutate(
      round_type = factor(
        round_type,
        levels = c(
          "Timed Training 1",
          "Timed Training 2",
          "Timed Training 3",
          "Qualifying",
          "Semi-Final",
          "Final"
        )
      )
    ) |>
    relocate(time, .after = `5`) |>
    arrange(round_type)

  gt(tbl, rowname_col = "round_type") |>
    sub_missing() |>
    opt_row_striping() |>
    cols_label("round_type" = "") |>
    fmt_number(!round_type, decimals = 2) |>
    tab_spanner(label = "Section", columns = !round_type) |>
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_stub()
    ) |>
    tab_header(
      md(
        glue::glue(
          "**{name}'s split times from {event_name}**"
        )
      ),
      md("Fastest splits are highlighted in green")
    ) |>
    highlight_fastest_splits(tbl)
}

fastest_splits_gt("Loic Bruni", "Fort William")
