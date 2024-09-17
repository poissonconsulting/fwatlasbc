roxygen2md::roxygen2md()

styler::style_pkg(
  scope = "line_breaks",
  filetype = c("R", "Rmd")
)

lintr::lint_package(linters = linters_with_defaults(
  line_length_linter = line_length_linter(1000),
  object_name_linter = object_name_linter(regexes = ".*"))
)

lintr::lint_package()

devtools::test()
devtools::document()

pkgdown::build_home()

devtools::check()
