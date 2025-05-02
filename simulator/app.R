# ------------------------------------------------------------------------------
# Load
# ------------------------------------------------------------------------------
library(shiny)
library(bslib)
library(gt)
library(readr)
library(purrr)
library(dplyr)
library(fs)
library(stringr)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------
sectors <- read_csv("sectors.csv", show_col_types = FALSE)
simulated <- read_csv("simulated.csv", show_col_types = FALSE)

fastest_split_by_run <- function(.data, section) {
  .data |>
    slice_min(.data[[section]], na_rm = TRUE) |>
    pull(round_type) |>
    as.character()
}

fastest_splits_highlight <- function(gt_tbl, data) {
  reduce(
    .x = paste0("section_", 1:5),
    .f = \(gt, x) {
      gt |>
        data_color(
          columns = x,
          rows = round_type == fastest_split_by_run(data, x),
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
    # relocate(time, .after = `5`) |>
    arrange(round_type)

  gt(tbl, rowname_col = "round_type") |>
    sub_missing() |>
    opt_row_striping() |>
    cols_label(
      "round_type" = "",
      "section_1" = "1",
      "section_2" = "2",
      "section_3" = "3",
      "section_4" = "4",
      "section_5" = "5"
    ) |>
    fmt_number(!round_type, decimals = 2) |>
    tab_spanner(label = "Section", columns = !c(round_type, time)) |>
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
    fastest_splits_highlight(tbl)
}

simulated_splits_ranked <- simulated |>
  select(name, event_name, starts_with("split_"), split_5 = time) |>
  mutate(
    across(
      starts_with("split_"),
      ~ rank(.x, ties.method = "min"),
      .names = "{.col}_rank"
    ),
    .by = event_name
  ) |>
  mutate(
    across(
      starts_with("split_") & !ends_with("_rank"),
      ~ .x - min(.x),
      .names = "{.col}_gap"
    ),
    .by = "event_name"
  ) |>
  mutate(
    event_name = factor(
      event_name,
      levels = c(
        "Fort William",
        "Bielsko-Biala",
        "Leogang",
        "Val di Sole",
        "Les Gets",
        "Loudenvielle",
        "Mont-Sainte-Anne"
      )
    )
  )

simulated_splits_merge_cols <- function(gt_tbl) {
  reduce(
    1:5,
    \(gt_tbl, x) {
      cols_merge(
        gt_tbl,
        columns = c(
          paste0("split_", x, "_gap"),
          paste0("split_", x, "_rank")
        ),
        pattern = "{1} ({2})"
      )
    },
    .init = gt_tbl
  )
}

simulated_splits_heat_map <- function(name, event_name) {
  simulated_splits_ranked |>
    filter(split_5_rank <= 10 | name == {{ name }}) |>
    filter(event_name == {{ event_name }}) |>
    select(name, ends_with("_gap"), ends_with("_rank")) |>
    gt() |>
    cols_label(
      name = "",
      split_1_gap = "Split 1",
      split_2_gap = "Split 2",
      split_3_gap = "Split 3",
      split_4_gap = "Split 4",
      split_5_gap = "Finish"
    ) |>
    data_color(
      columns = ends_with("_gap"),
      palette = c("#4daf4a", "#ffffbf", "#e41a1c")
    ) |>
    text_transform(
      fn = \(x) if_else(x == "0.000", paste0(x), paste("+", x)),
      locations = cells_body(columns = ends_with("_gap"))
    ) |>
    simulated_splits_merge_cols() |>
    tab_style(
      style = cell_borders(sides = "all", style = "solid", color = "#e9e9e9"),
      locations = cells_body(
        columns = ends_with("_gap")
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    opt_row_striping() |>
    tab_options(
      table.font.size = px(12),
      column_labels.font.weight = "bold"
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = !name)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "name")
    ) |>
    tab_header(
      title = md(
        glue::glue(
          "**Simulated split times from {event_name} for the top 10 + {name}**"
        )
      ),
      subtitle = md(
        "Each split in each race is colored by split time from fastest (green) to slowest (red)"
      )
    )
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  theme = bs_theme(version = 5),
  title = "Simulator",
  sidebar = sidebar(
    "Controls:",
    selectInput(
      "name",
      "Select rider",
      unique(sectors$name)
    ),
    selectInput(
      "event_name",
      "Select event",
      unique(sectors$event_name)
    ),
    selectizeInput(
      "name_additional",
      "Optionally, select up to five additional riders for comparison",
      choices = NULL,
      multiple = TRUE,
      options = list(maxItems = 5, placeholder = "Select comparison riders...")
    )
  ),
  layout_columns(
    card(gt_output("fastest_splits_tbl"), full_screen = TRUE),
    card(gt_output("simulated_splits_tbl"), full_screen = TRUE)
  )
)

# --------------------------------------------------------------x----------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  observe({
    input$name
  }) |>
    bindEvent(
      updateSelectizeInput(
        session = session,
        "name_additional",
        choices = setdiff(unique(sectors$name), input$name)
      )
    )

  output$fastest_splits_tbl <- gt::render_gt(
    fastest_splits_gt(input$name, input$event_name)
  )
  output$simulated_splits_tbl <- gt::render_gt(
    simulated_splits_heat_map(input$name, input$event_name)
  )
}

# ------------------------------------------------------------------------------
# Run
# ------------------------------------------------------------------------------
shinyApp(ui, server)
