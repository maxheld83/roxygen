test_that("detect_line_ending works", {
  # write files with various newlines
  win_nl <- tempfile()
  on.exit(unlink(win_nl), add = TRUE, after = FALSE)

  unix_nl <- tempfile()
  on.exit(unlink(unix_nl), add = TRUE, after = FALSE)

  non_existent_file <- tempfile()

  empty_file <- tempfile()
  file.create(empty_file)
  on.exit(unlink(empty_file), add = TRUE, after = FALSE)

  base::writeLines(c("foo", "bar"), win_nl, sep = "\r\n")
  base::writeLines(c("foo", "bar"), unix_nl, sep = "\n")
  expect_equal(detect_line_ending(win_nl), "\r\n")
  expect_equal(detect_line_ending(unix_nl), "\n")
  expect_equal(detect_line_ending(non_existent_file), "\n")
  expect_equal(detect_line_ending(empty_file), "\n")
})

test_that("write_lines writes windows newlines for files with windows newlines, and unix newlines otherwise", {
  # write files with various newlines
  win_nl <- tempfile()
  on.exit(unlink(win_nl), add = TRUE, after = FALSE)

  unix_nl <- tempfile()
  on.exit(unlink(unix_nl), add = TRUE, after = FALSE)

  non_existent_file <- tempfile()

  empty_file <- tempfile()
  file.create(empty_file)
  on.exit(unlink(empty_file), add = TRUE, after = FALSE)

  base::writeLines(c("foo", "bar"), win_nl, sep = "\r\n")

  base::writeLines(c("foo", "bar"), unix_nl, sep = "\n")

  write_lines("baz", win_nl)
  expect_equal(readChar(win_nl, 100), "baz\r\n")

  write_lines("baz", unix_nl)
  expect_equal(readChar(unix_nl, 100), "baz\n")

  write_lines("baz", non_existent_file)
  expect_equal(readChar(non_existent_file, 100), "baz\n")

  write_lines("baz", empty_file)
  expect_equal(readChar(empty_file, 100), "baz\n")
})
