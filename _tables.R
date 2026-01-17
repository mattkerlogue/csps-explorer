csps_benchmarks <- arrow::read_parquet(
  "data/01-benchmarks/csps_benchmarks_2009-2024.parquet"
)

csps_benchmarks |>
  dplyr::filter(
    (year == 2023 | year == 2024) &
      (grepl(
        "^2|^3|^5\\.01|^7\\.02|^6\\.01\\.001\\.00|^6\\.02\\.001\\.00",
        uid_qm_num
      )) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  tidyr::drop_na(yr_2023, yr_2024) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = yr_2024 - yr_2023,
  ) |>
  dplyr::slice_max(diff, n = 5) |>
  dplyr::arrange(-diff) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    question_out = glue::glue("{label} (% {response_category})"),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1))
  ) |>
  dplyr::select(question_out, yr_2023, yr_2024, diff) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()

csps_benchmarks |>
  dplyr::filter(
    (year == 2023 | year == 2024) &
      (grepl(
        "^2|^3|^5\\.01|^7\\.02|^6\\.01\\.001\\.00|^6\\.02\\.001\\.00",
        uid_qm_num
      )) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  tidyr::drop_na(yr_2023, yr_2024) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = yr_2024 - yr_2023,
  ) |>
  dplyr::filter(diff < 0) |>
  dplyr::slice_min(diff, n = 5) |>
  dplyr::arrange(diff) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    question_out = glue::glue("{label} (% {response_category})"),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1))
  ) |>
  dplyr::select(question_out, yr_2023, yr_2024, diff) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()

csps_benchmarks |>
  dplyr::filter(
    grepl("^2", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    question_out = glue::glue("{label} (% {response_category})"),
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon)
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(uid_qm_txt, question_out, yr_2023, yr_2024, bar_chart, diff) |>
  knitr::kable(
    format = "pipe",
    col.names = c("uid", "Question", "2023", "2024", "", "Difference"),
    align = c("l", "l", "r", "r", "l", "r")
  ) |>
  stringr::str_squish() |>
  gsub("\\|", "| ", x = _) |>
  gsub("  ", " ", x = _) |>
  clipr::write_clip()


csps_benchmarks |>
  dplyr::filter(
    grepl("^4\\.01\\.001", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff == 0 ~ "=",
      diff > 0 ~ "\u25b2",
      diff < 0 ~ "\u25bc"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus")
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(label, `2023` = yr_2023, `2024` = yr_2024, diff, change_icon) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()


csps_benchmarks |>
  dplyr::filter(
    grepl("^4\\.01\\.002", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon)
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(label, `2023` = yr_2023, `2024` = yr_2024, bar_chart, diff) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()


csps_benchmarks |>
  dplyr::filter(
    uid_qm_num == "6.01.001.00" &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::mutate(
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
    uid_qm_num = dplyr::case_match(
      response_category,
      "yes" ~ "6.01.001.01",
      "no" ~ "6.01.001.02",
      "ptns" ~ "6.01.001.03"
    ),
    response_category = stringr::str_to_sentence(response_category)
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(
    response_category,
    `2023` = yr_2023,
    `2024` = yr_2024,
    bar_chart,
    diff
  ) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()

csps_benchmarks |>
  dplyr::filter(
    grepl("6.01.002", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(label, `2023` = yr_2023, `2024` = yr_2024, bar_chart, diff) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()


csps_benchmarks |>
  dplyr::filter(
    uid_qm_num == "6.02.001.00" &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::mutate(
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
    uid_qm_num = dplyr::case_match(
      response_category,
      "yes" ~ "6.02.001.01",
      "no" ~ "6.02.001.02",
      "ptns" ~ "6.02.001.03"
    ),
    response_category = stringr::str_to_sentence(response_category)
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(
    response_category,
    `2023` = yr_2023,
    `2024` = yr_2024,
    bar_chart,
    diff
  ) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()

csps_benchmarks |>
  dplyr::filter(
    grepl("6.02.003", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(label, `2023` = yr_2023, `2024` = yr_2024, bar_chart, diff) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()

csps_benchmarks |>
  dplyr::filter(
    uid_qm_num == "6.02.004.00" &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::mutate(
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
    uid_qm_num = dplyr::case_match(
      response_category,
      "yes" ~ "6.02.001.01",
      "no" ~ "6.02.001.02",
      "ptns" ~ "6.02.001.03"
    ),
    response_category = stringr::str_to_sentence(response_category)
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(
    response_category,
    `2023` = yr_2023,
    `2024` = yr_2024,
    bar_chart,
    diff
  ) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()

csps_benchmarks |>
  dplyr::filter(
    grepl("6.03.002", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(label, `2023` = yr_2023, `2024` = yr_2024, bar_chart, diff) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()


csps_benchmarks |>
  dplyr::filter(
    grepl("^6\\.04", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark" &
      response_category == "yes"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(label, `2023` = yr_2023, `2024` = yr_2024, bar_chart, diff) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()


csps_benchmarks |>
  dplyr::filter(
    grepl("^7\\.02", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    question_out = paste0(label, " (% ", response_category, ")"),
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(
    question_out,
    `2023` = yr_2023,
    `2024` = yr_2024,
    bar_chart,
    diff
  ) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()

csps_benchmarks |>
  dplyr::filter(
    grepl("^7\\.01", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(label, `2023` = yr_2023, `2024` = yr_2024, bar_chart, diff) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()

csps_benchmarks |>
  dplyr::filter(
    grepl("^7\\.03", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    question_out = paste0(label, " (% ", response_category, ")"),
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(
    question_out,
    `2023` = yr_2023,
    `2024` = yr_2024,
    bar_chart,
    diff
  ) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()


csps_benchmarks |>
  dplyr::filter(
    grepl("^5", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    question_out = paste0(label, " (% ", response_category, ")"),
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(
    question_out,
    `2023` = yr_2023,
    `2024` = yr_2024,
    bar_chart,
    diff
  ) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()

csps_benchmarks |>
  dplyr::filter(
    grepl("^3", uid_qm_num) &
      (year == 2023 | year == 2024) &
      data_type == "benchmark"
  ) |>
  dplyr::mutate(year = paste0("yr_", year)) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(
    across(c(yr_2023, yr_2024), ~ round(.x, 1)),
    diff = janitor::round_half_up(yr_2024 - yr_2023, 1),
  ) |>
  dplyr::left_join(qs_ref, by = c("uid_qm_num", "uid_qm_txt")) |>
  dplyr::mutate(
    question_out = paste0(label, " (% ", response_category, ")"),
    bar_chart = glue::glue(
      "<span class=\"micro-chart\"><span class=\"micro-bar\" data-value=\"{janitor::round_half_up(yr_2024)}\"></span>"
    ),
    across(c(yr_2023, yr_2024), ~ scales::percent(.x, 0.1, scale = 1)),
    change_icon = dplyr::case_when(
      diff ==
        0 ~ "{{< iconify not-available title=\"Score unchanged icon\" label=\"Score unchanged icon\" >}}",
      diff >
        0 ~ "{{< iconify triangle-solid title=\"Score increased icon\" label=\"Score increased icon\" style=\"color:#007d79\" >}}",
      diff <
        0 ~ "{{< iconify triangle-down-solid title=\"Score decreased icon\" label=\"Score decreased icon\" style=\"color:#da1e28\" >}}"
    ),
    diff = scales::number(diff, 0.1, style_positive = "plus"),
    diff = dplyr::if_else(diff == "0.0", "±0.0", diff),
    diff = paste(diff, change_icon),
  ) |>
  dplyr::arrange(uid_qm_num) |>
  dplyr::select(
    uid_qm_num,
    question_out,
    `2023` = yr_2023,
    `2024` = yr_2024,
    bar_chart,
    diff
  ) |>
  knitr::kable(format = "pipe") |>
  stringr::str_squish() |>
  clipr::write_clip()
