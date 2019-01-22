reconcileGeographies <- function(polyA, polyB,
                                 idA = NULL, idB = NULL,
                                 project_crs = NULL,
                                 dist_buffer = 5) {

  original_polyA <- polyA
  original_polyB <- polyB

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

  # Missing
  missing_A <-
    polyA$`.unigeokey`[!polyA$`.unigeokey` %in%
                         all_combinations$unigeokey_A]
  missing_B <-
    polyB$`.unigeokey`[!polyB$`.unigeokey` %in%
                         all_combinations$unigeokey_B]

  # 4: A intersects B
  res4 <-
    sf::st_intersects(polyA %>%
                        dplyr::filter(`.unigeokey` %in% missing_A),
                      polyB,
                      sparse = FALSE)
  these_combinations4 <- data.frame()
  if (!is.null(res4)) {
    rownames(res4) <-
      paste0("s", polyA$`.unigeokey`[polyA$`.unigeokey` %in% missing_A])
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
  }

  # B intersects A
  res5 <-
    sf::st_intersects(polyB %>%
                        dplyr::filter(`.unigeokey` %in% missing_B &
                                        !(`.unigeokey` %in% these_combinations4$unigeokey_B)),
                      polyA,
                      sparse = FALSE)

  these_combinations5 <- data.frame()
  if (!is.null(dim(res5))) {
    rownames(res5) <-
      paste0("s", polyB$`.unigeokey`[polyB$`.unigeokey` %in% missing_B])
    colnames(res5) <-
      paste0("s", polyA$`.unigeokey`)
    these_combinations5 <- reshape2::melt(res5)
    these_combinations5 <- these_combinations5[these_combinations5$value,]
    these_combinations5$value <- NULL
    colnames(these_combinations5) <- c("unigeokey_B", "unigeokey_A")
    these_combinations5$unigeokey_A <-
      gsub("^s", "", these_combinations5$unigeokey_A)
    these_combinations5$unigeokey_B <-
      gsub("^s", "", these_combinations5$unigeokey_B)
    these_combinations5$type <- 'BintersectsA'

  }

  all_combinations <-
    rbind(all_combinations,
          rbind(these_combinations4, these_combinations5))

  sum(!polyA$.unigeokey %in% all_combinations$unigeokey_A)
  sum(!polyB$.unigeokey %in% all_combinations$unigeokey_B)

  return(all_combinations)

}


reconcilePoly <- function(from, to) {

  pnts <-
    lapply(1:nrow(from), FUN = function(i)
      coordinates(sp::spsample(from[i,], 1,
                               type = 'random', iter=10)))
  df <-
    data.frame(matrix(unlist(pnts), nrow=nrow(from),
                      byrow=T))

  pnts.sp <-
    sp::SpatialPoints(df,
                      proj4string = CRS(proj4string(from)))

  res <-
    over(sp::spTransform(pnts.sp, sp::proj4string(to)), to)

  return(res)
}





