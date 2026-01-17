org_ref <- readr::read_csv(
  "../csps-data/proc/csps_org_regex.csv",
  show_col_types = FALSE
)

csps_orgs <- arrow::read_parquet(
  "../csps-data/data/02-organisations/csps_organisations_2009-2024_df950add.parquet"
)

opt_tag <- function(value, label, year = NULL) {
  if (is.null(year)) {
    htmltools::tags$option(
      value = value,
      label
    )
  } else {
    htmltools::tags$option(
      value = paste(year, value, sep = "/"),
      label
    )
  }
}

csps_orgs_list <- csps_orgs |>
  dplyr::filter(uid_org_txt != "CSBNCH" & uid_org_txt != "CSMEAN") |>
  dplyr::distinct(year, uid_org_txt) |>
  dplyr::left_join(org_ref, by = "uid_org_txt") |>
  dplyr::mutate(
    raw_tag = purrr::map2(
      .x = uid_org_txt,
      .y = organisation_name,
      .f = ~ opt_tag(.x, .y)
    ),
    year_tag = purrr::pmap(
      .l = list(value = uid_org_txt, label = organisation_name, year = year),
      .f = opt_tag
    )
  ) |>
  dplyr::arrange(organisation_name)

year_select <- function(year, tags_list) {
  if (year == "all") {
    tags <- tags_list |>
      dplyr::filter(year == max(year), .by = uid_org_txt) |>
      dplyr::pull(raw_tag)
  } else {
    tags <- tags_list[tags_list$year == year, "year_tag"][[1]]
  }

  htmltools::tags$select(
    id = "org-select",
    name = "organisations",
    `aria-label` = "Select an organisation",
    htmltools::tags$option(
      value = "",
      "Select an organisation"
    ),
    tags
  )
}

org_select_form <- function(year, tags_list) {
  htmltools::tags$form(
    id = "org-select-form",
    class = "mb-5",
    method = "dialog",
    year_select(year, tags_list),
    htmltools::tags$input(
      type = "submit",
      id = "org-results-go",
      class = "action-button",
      value = "Go"
    )
  )
}

md_block <- function(year, tags_list) {
  c(
    "```{=html}",
    as.character(org_select_form(year, tags_list)),
    "```"
  )
}

write_html_partial <- function(
  year,
  path = "partials/org_select",
  tags_list = csps_orgs_list
) {
  html_partial <- as.character(org_select_form(year, tags_list))
  full_path <- file.path(path, paste0("org_select_", year, ".html"))
  writeLines(html_partial, full_path, sep = "\n")
}

write_html_partial("all")

purrr::walk(
  .x = 2009:2024,
  .f = write_html_partial
)
