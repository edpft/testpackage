#' Title
#'
#' @param url
#' @param zipped
#' @param ext
#'
#' @return
#' @export
#'
#' @examples
url2sf <-
  function(url, zipped = TRUE, ext) {

    if (zipped) {
      tmp <-
        tempfile(
          fileext = ".zip"
        )
    } else {
      tmp <-
        tempfile(
          fileext = ext
        )
    }

    utils::download.file(
      url = url,
      destfile = tmp,
      method = "auto",
      mode = "wb"
      )

    if (zipped) {
      utils::unzip(
        zipfile = tmp,
        exdir = tempdir()
      )
    }

    sf::st_as_sf(
      list.files(
        path = tempdir(),
        pattern = ext,
        full.names = TRUE
      )
    )
  }
