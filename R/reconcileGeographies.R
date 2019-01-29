#' Reconcile geographies (features) from two spatial polygon objects.
#'
#' @param polyA The first spatial polygon object.
#' @param polyB The second spatial polygon object.
#' @param idA The name of the id column in the first object. If not provided, the first column is assume to be the ID.
#' @param idB The name of the id column in the second object. If not provided, the first column is assume to be the ID.
#' @param project_crs The EPSG coordinate system. The unit must be the metre.
#' @param dist_buffer The distance for the buffer. Default is 5 metres.
#' @param min_inters_area The minimum intersection area for to two geometries to be intersecting. Default is 1 m2.
#' @return A data.frame relating the geographies in the two spatial objects (one-to-one, one-to-many) or many-to-many.
#' @examples
#' data(polygons, package = "recogeo")
#' res <- reconcileGeographies(poly_a, poly_b)
reconcileGeographies <- function(polyA, polyB,
                                 idA = NULL, idB = NULL,
                                 project_crs = NULL,
                                 dist_buffer = 5,
                                 min_inters_area = 1) {

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

  if(is.null(idA)) {
    idA <- colnames(polyA)[1]
  }
  if(is.null(idB)) {
    idB <- colnames(polyB)[1]
  }

  if (is.null(project_crs)) {
    project_crs <- sf::st_crs(polyB)
  }

  polyA[['.unigeokey']] <- as.character(polyA[[idA]])
  polyB[['.unigeokey']] <- as.character(polyB[[idB]])

  polyA <- sf::st_transform(polyA, crs=project_crs)
  polyB <- sf::st_transform(polyB, crs=project_crs)

  if (!grepl("units\\=m ", as.character(sf::st_crs(polyA))[2])) {
    stop("CRS units must meters. Indicate CRS with `project_crs`.")
  }

  # 1: A Equals B
  res1a <-
    sf::st_contains(polyA %>%
                      sf::st_buffer(dist_buffer),
                    polyB,
                    sparse = FALSE)

  res1b <-
    sf::st_within(polyA,
                  polyB %>%
                    sf::st_buffer(dist_buffer),
                  sparse = FALSE)

  res1 <- res1a == TRUE & res1b == TRUE

  rownames(res1) <- paste0("s", polyA$`.unigeokey`)
  colnames(res1) <- paste0("s", polyB$`.unigeokey`)

  require(reshape2)
  these_combinations1 <- melt(res1)
  these_combinations1 <- these_combinations1[these_combinations1$value,]
  these_combinations1$value <- NULL
  colnames(these_combinations1) <- c("unigeokey_A", "unigeokey_B")
  these_combinations1$unigeokey_A <-
    gsub("^s", "", these_combinations1$unigeokey_A)
  these_combinations1$unigeokey_B <-
    gsub("^s", "", these_combinations1$unigeokey_B)

  these_combinations1$type <- 'same'

  # 2: A Contains B
  polyA <-
    polyA %>%
    dplyr::filter(!`.unigeokey` %in%
                    these_combinations1$unigeokey_A)
  polyB <-
    polyB %>%
    dplyr::filter(!`.unigeokey` %in%
                    these_combinations1$unigeokey_B)

  res2 <-
    sf::st_contains(polyA %>%
                      sf::st_buffer(dist_buffer),
                    polyB,
                    sparse = FALSE)
  rownames(res2) <- paste0("s", polyA$`.unigeokey`)
  colnames(res2) <- paste0("s", polyB$`.unigeokey`)

  these_combinations2 <- reshape2::melt(res2)
  these_combinations2 <- these_combinations2[these_combinations2$value,]
  these_combinations2$value <- NULL
  colnames(these_combinations2) <- c("unigeokey_A", "unigeokey_B")
  these_combinations2$unigeokey_A <-
    gsub("^s", "", these_combinations2$unigeokey_A)
  these_combinations2$unigeokey_B <-
    gsub("^s", "", these_combinations2$unigeokey_B)
  these_combinations2$type <- 'AcontainsB'

  # 3: B Contains A
  res3 <-
    sf::st_within(polyA,
                  polyB %>%
                    st_buffer(5),
                  sparse = FALSE)
  rownames(res3) <- paste0("s", polyA$`.unigeokey`)
  colnames(res3) <- paste0("s", polyB$`.unigeokey`)

  require(reshape2)
  these_combinations3 <- reshape2::melt(res3)
  these_combinations3 <- these_combinations3[these_combinations3$value,]
  these_combinations3$value <- NULL
  colnames(these_combinations3) <- c("unigeokey_A", "unigeokey_B")
  these_combinations3$unigeokey_A <-
    gsub("^s", "", these_combinations3$unigeokey_A)
  these_combinations3$unigeokey_B <-
    gsub("^s", "", these_combinations3$unigeokey_B)
  these_combinations3$type <- 'BcontainsA'

  # Combines
  all_combinations <-
    rbind(these_combinations1, these_combinations2)
  all_combinations <-
    rbind(all_combinations, these_combinations3)

  # 4: A intersects B
  res4 <-
    sf::st_intersects(polyA,
                      polyB,
                      sparse = FALSE)
  these_combinations4 <- data.frame()
  if (!is.null(res4)) {
    rownames(res4) <-
      paste0("s", polyA$`.unigeokey`)
    colnames(res4) <-
      paste0("s", polyB$`.unigeokey`)
    these_combinations4 <- reshape2::melt(res4)
    these_combinations4 <- these_combinations4[these_combinations4$value,]
    these_combinations4$value <- NULL
    colnames(these_combinations4) <- c("unigeokey_A", "unigeokey_B")
    these_combinations4$unigeokey_A <-
      gsub("^s", "", these_combinations4$unigeokey_A)
    these_combinations4$unigeokey_B <-
      gsub("^s", "", these_combinations4$unigeokey_B)
    these_combinations4$type <- 'AintersectsB'

    is_intersecting <-
      mapply(testIntersectionArea,
             these_combinations4$unigeokey_A,
             these_combinations4$unigeokey_B,
             MoreArgs =
               list(polyA, polyB, min_inters_area))

    these_combinations4 <-
      these_combinations4[is_intersecting,]

    these_combinations4 <-
      these_combinations4[!paste0(these_combinations4$unigeokey_A,
                                 "|",
                                 these_combinations4$unigeokey_B) %in%
                            paste0(all_combinations$unigeokey_A,
                                   "|",
                                   all_combinations$unigeokey_B),]
    }


  all_combinations <-
    rbind(all_combinations, these_combinations4)

  final_check_a <-
    polyA$.unigeokey[!polyA$.unigeokey %in% all_combinations$unigeokey_A]
  if(length(final_check_a)>0) {
    warning(paste0("Warning: Geographies from the first spatial object not related to ",
                   "any geography in the second spatial object: ",
                   paste(final_check_a, collapse = ", "), "."))
  }
  final_check_b <-
    polyB$.unigeokey[!polyB$.unigeokey %in% all_combinations$unigeokey_B]
  if(length(final_check_b)>0) {
    warning(paste0("Warning: Geographies from the second spatial object not related to ",
                   "any geography in the first spatial object: ",
                   paste(final_check_b, collapse = ", "), "."))
  }

  return(all_combinations)

}

#' Test that the intersection area of two geometries is larger than a minimum
#'
#' @param unigeokey_A The unique ID of the geometry in the first spatial object.
#' @param unigeokey_B The unique ID of the geometry in the second spatial object.
#' @param polyA The first spatial object.
#' @param polyB The second spatial object.
#' @param min_inters_area The minimum intersecting area.
#' @return A logical vector.
#' @examples
#' mapply(testIntersectionArea, unigeokeys_A, unigeokeys_B, MoreArgs = list(polyA, polyB, min_inters_area))
testIntersectionArea <- function(unigeokey_A,
                                 unigeokey_B,
                                 polyA,
                                 polyB,
                                 min_inters_area) {
  intersection_area <-
    st_area(
      st_intersection(st_geometry(polyA[polyA$.unigeokey == unigeokey_A,]),
                      st_geometry(polyB[polyB$.unigeokey == unigeokey_B,]))
    )
  return(as.numeric(intersection_area) > min_inters_area)
}

#' Reconcile data based on results from reconcileGeographies().
#' @param res Results from reconcileGeographies().
#' @param dataA The first data object. Either a spatial object with a data.frame or a data.frame.
#' @param dataB The second data object. Either a spatial object with a data.frame or a data.frame.
#' @param idA The name of the id column in the first object. If not provided, the first column is assume to be the ID.
#' @param idB The name of the id column in the second object. If not provided, the first column is assume to be the ID.
#' @param varA An ordered character vector with the names of the variables to concile from the first spatial object.
#' @param varB An ordered character vector with the names of the variables to concile from the second spatial object.
#' @param return_spatial Whether to return a spatial object (set to "A" or "B") or a simple data.frame (set to FALSE). "A" or "B" determines which geometries to return, either from the first or the seconf spatial object.
#' @return A Simple Feature with a data.frame or only a data.frame.
#' @example
#'
reconcileData <- function(res, dataA, dataB,
                          idA = NULL, idB = NULL,
                          varA, varB,
                          return_spatial = "A") {

  if(length(varA) != length(varB)) {
    stop("The length of the variable name vector differ.")
  }

  if(is.null(idA)) {
    idA <- colnames(dataA)[1]
  }
  if(is.null(idB)) {
    idB <- colnames(dataB)[1]
  }

  reconciliation <-
    getUniqueReconciliationKey(res)

  # dataA <- poly_a
  # dataB <- poly_b

  dataA[['.unigeokey']] <- as.character(dataA[[idA]])
  dataB[['.unigeokey']] <- as.character(dataB[[idB]])

  if (return_spatial != FALSE) {
    return_spatial <- toupper(return_spatial)
  }

  if (return_spatial == FALSE) {

    if (class(dataA)[1] == 'sf') {
      st_geometry(dataA) <- NULL
    }
    if (class(dataB)[1] == 'sf') {
      st_geometry(dataB) <- NULL
    }
    if (class(dataA)[1] == 'SpatialPolygonsDataFrame') {
      dataA <- dataA@data
    }
    if (class(dataB)[1] == 'SpatialPolygonsDataFrame') {
      dataA <- dataB@data
    }

  } else if (return_spatial == "A") {

    if (class(dataA)[1] != 'sf'){
      if (class(dataA)[1] == 'SpatialPolygonsDataFrame') {
        dataA <- sf::st_as_sf(dataA)
      } else {
        stop("dataA is not a spatial object of a supported class")
      }
    }
    if (class(dataB)[1] == 'sf') {
      st_geometry(dataB) <- NULL
    }
    if (class(dataB)[1] == 'SpatialPolygonsDataFrame') {
      dataA <- dataB@data
    }

  } else if (return_spatial == "B") {

    if (class(dataB)[1] != 'sf'){
      if (class(dataB)[1] == 'SpatialPolygonsDataFrame') {
        dataB <- sf::st_as_sf(dataB)
      } else {
        stop("dataB is not a spatial object of a supported class")
      }
    }
    if (class(dataA)[1] == 'sf') {
      st_geometry(dataA) <- NULL
    }
    if (class(dataA)[1] == 'SpatialPolygonsDataFrame') {
      dataA <- dataA@data
    }
  } else {
    stop("return_spatial argument not recognised.")
  }

  dataA_recogeo <-
    merge(dataA,
          reconciliation[reconciliation$set == 'A', 1:2],
          by.x = '.unigeokey', by.y = '.unigeokey_old',
          all.x = FALSE) %>%
    dplyr::group_by(`.unigeokey_new`) %>%
    dplyr::summarize_at(.vars = vars(varA), .funs = funs("sum"))
  dataB_recogeo <-
    merge(dataB,
          reconciliation[reconciliation$set == 'B', 1:2],
          by.x = '.unigeokey', by.y = '.unigeokey_old',
          all.x = FALSE) %>%
    dplyr::group_by(`.unigeokey_new`) %>%
    dplyr::summarize_at(.vars = vars(varB), .funs = funs("sum"))

  if (return_spatial == FALSE) {
    new_data_recogeo <-
      merge(dataA_recogeo[,c(".unigeokey_new",varA)],
            dataB_recogeo[,c(".unigeokey_new",varB)],
            by = ".unigeokey_new", suffix = c("_A","_B"))
  } else if (return_spatial == "A") {
    new_data_recogeo <-
      merge(dataA_recogeo[,c(".unigeokey_new",varA)],
            dataB_recogeo[,c(".unigeokey_new",varB)],
            by = ".unigeokey_new", suffix = c("_A","_B"))
  } else {
    new_data_recogeo <-
      merge(dataB_recogeo[,c(".unigeokey_new",varB)],
            dataA_recogeo[,c(".unigeokey_new",varA)],
            by = ".unigeokey_new", suffix = c("_B","_A"))
  }
  return(new_data_recogeo)
}

#' Get a unique reconciliation key based on results from reconcileGeographies().
#'
#' @param res Results from reconcileGeographies().
#' @return A data.frame.
#' @example
#' unirecogeokey_df <- getUniqueReconciliationKey(res)
getUniqueReconciliationKey <- function(res) {
  el  <- cbind(A=paste0(res$unigeokey_A,"~A"),
               B=paste0(res$unigeokey_B,"~B"))
  g <- igraph::graph_from_edgelist(el, directed = FALSE)
  comp <- igraph::components(g)
  reconciliation <- data.frame(`.unigeokey_old` = names(comp$membership),
                               `.unigeokey_new` = comp$membership,
                               row.names = NULL)
  reconciliation$set <-
    gsub("^.*~", "", reconciliation$`.unigeokey_old`)
  reconciliation$`.unigeokey_old` <-
    gsub("~.*$", "", reconciliation$`.unigeokey_old`)
  return(reconciliation)
}

