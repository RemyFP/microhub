source(test_path("../../R/validate_outside_model.R"))

test_that("uploaded forecasts combine with built-in forecasts and each other", {
  reference <- tibble(
    model = "Regular Baseline",
    reference_date = as.Date("2026-10-17"),
    horizon = 1,
    target_end_date = as.Date("2026-10-24"),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = c("0.025", "0.5", "0.975"),
    value = c(10, 20, 30)
  )
  csv_path <- withr::local_tempfile(fileext = ".csv")
  uploaded <- reference |>
    mutate(model = "Outside Model", output_type_id = c("0.025", "0.500", "0.975"))
  write_csv(uploaded, csv_path)

  result <- validate_outside_model(csv_path, reference)

  expect_length(result$errors, 0)
  expect_length(result$warnings, 0)
  expect_identical(result$data$output_type_id, reference$output_type_id)
  expect_s3_class(result$data$reference_date, "Date")
  expect_s3_class(result$data$target_end_date, "Date")

  second_upload <- result$data |> mutate(model = "Second Outside Model")
  combined <- bind_rows(reference, bind_rows(list(result$data, second_upload)))
  expect_equal(nrow(combined), 9L)

  ensemble <- combined |>
    summarize(
      value = median(value),
      .by = c(reference_date, horizon, target_end_date,
              target_group, output_type, output_type_id)
    )
  expect_equal(nrow(ensemble), 3L)
  expect_equal(ensemble$value, reference$value)
})
