test_that("Does not work outside interactive session.", {
  expect_error({
    with_interactive({
    create_directory_structure(c("A", "B", "C"),
                                           c("One", "TWO", "THREE"),
                                           test_path())
  }, value = F )},
    "This function must be run in interactive session." )
})

# test_that("Creates directory structure needed",
#           {
#
#
#             tmd <- tempdir()
#             on.exit(unlink(tmd))
#             plots <- c("A", "B", "C")
#             sites <- purrr::pmap_chr(expand.grid(.x =plots,.y =1:3), ~glue::glue("{.x}_{.y}"))
#             td <- glue::glue("{tmd}/TestStructure/")
#             dir.create(td)
#             # withr::defer(fs::dir_delete(tmd))
#             # mockr::local_mock(menu = function(choices,graphics = FALSE, title=NULL) 1)
#             mockr::with_mock(menu = function(choices,graphics = FALSE, title=NULL) 1,
#                              {with_interactive(create_directory_structure(plots,
#                                        sites,
#                                        td ), value =T) })
#             out <- list.dirs(td) |> sort()
#             expect_equal(
#               sum(purrr::map_int(sites, ~sum(grepl(.x, out)))),
#               length(sites) )
#
#             }
#
#
#           )
