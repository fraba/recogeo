#' Render a report comparing the reconciled geometries.
#'
#' @param res The result from reconcileGeometries().
#' @param polyA The first spatial polygon object passed to reconcileGeometries().
#' @param polyB The second spatial polygon object passed to reconcileGeometries().
#' @param output The type of document to produce: either 'html', 'pdf' or 'word'.
#' @param type A character vector with one or more type of reconciliations to print out: 'intersects', 'contains' and 'same'.
#' @return A data.frame relating the geographies in the two spatial objects (one-to-one, one-to-many) or many-to-many.
#' @examples
#' reportReconciledGeographies(res, polyA, polyB)
reportReconciledGeographies <- function(res,
                                        polyA, polyB,
                                        output = 'html',
                                        type = c("intersects",
                                                 "contains")) {

  output <- tolower(output)
  if (!output %in% c("pdf","html","word")) {
    stop(sprintf("Output %s not supported", output))
  }

  type <- tolower(type)
  if (any(!type %in% c("intersects","contains","same"))) {
    stop("Type not supported (must be \"intersects\", \"contains\" or \"same\")")
  }

  if (class(polyA)[1] != 'sf'){
    if (class(polyA)[1] == 'SpatialPolygonsDataFrame') {
      polyA <- sf::st_as_sf(polyA)
    } else {
      stop("polyA is not a spatial object of a supported class")
    }
  }

  if (class(polyB)[1] != 'sf'){
    if (class(polyB)[1] == 'SpatialPolygonsDataFrame') {
      polyB <- sf::st_as_sf(polyB)
    } else {
      stop("polyB is not a spatial object of a supported class")
    }
  }

  tmpdir_name <-
    paste0("tmp-",
           paste(replicate(15, sample(LETTERS, 1)), collapse=""))

  dir.create(tmpdir_name)

  rmd_header <-
    c("---",
      "title: \"Report on reconciled geographies\"",
      sprintf("output: %s_document", output),
      "---")

  rmd_setup <-
    c("```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE)",
      "library(ggplot2)",
      "library(gridExtra)",
      "library(sf)",
      sprintf("load(\"%s/%s/tmp.RData\")", getwd(), tmpdir_name),
      "```")

  res_to_print <-
    res[grepl(paste(type, collapse = "|"), res$type),]

  rmd_complete <-
    c(rmd_header, "", rmd_setup, "")

  for (i in 1:nrow(res_to_print)){
    rmd_complete <-
      c(rmd_complete,
        typesetFigure(i,
                      getCentroid(
                        polyA[res_to_print$unigeokey_A[i],],
                        polyB[res_to_print$unigeokey_B[i],]
                      ),
                      res_to_print$unigeokey_A[i],
                      res_to_print$unigeokey_B[i],
                      type = res_to_print$type[i]))
  }

  save(polyA, polyB, res_to_print,
       file = paste0(tmpdir_name, "/tmp.RData"))

  file_name <-
    paste0(tmpdir_name,
           sprintf("/recogeo-report-%s.Rmd",
                   format(Sys.Date(), "%F")))

  fileConn <- file(file_name)
  writeLines(rmd_complete, fileConn)
  close(fileConn)

  system(sprintf("R -e 'rmarkdown::render(\"%s\", output_dir = \"%s\")'",
                 file_name,
                 getwd()))

  unlink(tmpdir_name, recursive = T)

}

#' Get the centroid from two geograhies.
#'
#' @param geoA The first geography.
#' @param geoB The second geography.
#' @return A character vector.
#' @examples
#' getCentroid(
#'  polyA[res_to_print$unigeokey_A[i],],
#'  polyB[res_to_print$unigeokey_B[i],]
#' )
getCentroid <- function(geoA, geoB) {
  centroid <-
    suppressMessages(
      suppressWarnings(
        sf::st_centroid(
          sf::st_geometry(
            sf::st_union(
              sf::st_transform(geoA, 4326),
              sf::st_transform(geoB, 4326)
            )
          )
        )
      )
    )
  chr <- as.character(centroid)
  return(paste(round(eval(parse(text=chr)),4), collapse = ", "))
}

#' Typeset the figure.
#'
#' @param i The number of figure.
#' @param coords The geographic coordinates of the figure.
#' @param Ai The index of the geography in the first spatial object.
#' @param Bi The index of the geography in the second spatial object.
#' @param type The type of reconciliation.
#' @return A character vector.
#' @example
#' typesetFigure(i, getCentroid(
#'  polyA[res_to_print$unigeokey_A[i],],
#'  polyB[res_to_print$unigeokey_B[i],
#' )
typesetFigure <- function(i, coords, Ai, Bi, type) {
  return(strsplit(
    sprintf(
      "```{r figure-%s, fig.cap = '%s, (%s)'}
  theme_set(theme_bw() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()))
  grid.arrange(
  arrangeGrob(
  ggplot() +
    geom_sf(data = polyA[%s,], fill = 'red', colour = 'red', alpha = .5) +
    labs(title = 'poly A (id = %s)'),
  ggplot() +
    geom_sf(data = polyB[%s,], fill = 'blue', colour = 'blue', alpha = .5) +
    labs(title = 'poly B (id = %s)'),
  ncol = 2, nrow = 1),
  ggplot() +
    geom_sf(data = polyA[%s,], fill = NA, colour = 'red', alpha = .5) +
    geom_sf(data = polyB[%s,], fill = NA, colour = 'blue', alpha = .5),
  nrow = 2
  )
```
***

", i, type, coords, Ai, Ai, Bi, Bi, Ai, Bi),
    "\\n")[[1]])
}
