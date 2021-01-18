
#' Example files
#'
#' @param example_file An example file in the 'ex' directory of the 'bsrto'
#'   package.
#'
#' @return An absolute path to the file
#' @export
#'
#' @examples
#' bs_example("CTD_98911_10P_11_DN.ODF")
#'
bs_example <- function(example_file) {
  stopifnot(length(example_file) == 1)
  full_path <- system.file("ex", example_file, package = "bsrto")
  if (full_path == "") {
    abort(glue("'{ example_file }' does not exist in package 'bsrto'"))
  }

  full_path
}
