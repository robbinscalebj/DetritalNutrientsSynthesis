# pulled this biome code from now defunct guillembagaria/ggbiome

gd_get_biomes_spdf <- function(merge_deserts = FALSE) {
  
  # STEP 0
  # Argument checks
  # Is merge_deserts logical?
  if (!(is.logical(merge_deserts))) {
    stop('merge_deserts must be logical.')
  }
  # Is merge_deserts NA?
  if (is.na(merge_deserts)) {
    stop('merge_deserts must be either TRUE or FALSE.')
  }
  
  # STEP 1
  # Create the data frame
  biomes_df <- data.frame(
    mat = c(
      29.339, 13.971, 15.371, 17.510, 24.131, 27.074, 28.915, 29.201, 29.339,
      13.971, -9.706, -7.572, 4.491, 17.510, 15.371, 13.971, 17.510, 4.491,
      -7.572, -9.706, -6.687, -0.949, 3.098, 7.147, 10.165, 13.918, 18.626,
      18.176, 17.510, 18.626, 13.918, 10.165,  7.147, 3.098, -0.949, 1.039,
      1.998, 2.444, 3.118, 4.446, 7.758, 12.614, 18.720, 18.637, 18.626, -0.949,
      -6.687, -4.395, -4.098, -1.592, 0.914, 4.155, 3.118, 2.444, 1.998, 1.039,
      -0.949, 18.720,  12.614, 7.758, 4.446, 3.118, 4.155, 15.716, 20.136,
      19.392, 18.720, 18.720, 19.392, 20.136, 22.278, 23.756, 24.199, 24.714,
      25.667, 26.105, 27.414, 27.772, 25.709, 21.736, 18.720, 17.510, 18.176,
      18.626, 18.637, 18.720, 21.736, 25.709, 27.772, 28.418, 28.915, 27.074,
      24.131, 17.510, -6.687, -8.896, -9.706, -13.382, -15.366, -15.217, -8.373,
      -4.098, -1.592, -4.098, -4.395, -6.687
    ),
    map = c(
      21.3, 23.0, 174.6, 535.1, 702.9, 847.9, 992.4, 532.1, 21.3, 23.0, 7.3,
      87.2, 314.6, 535.1, 174.6, 23.0, 535.1, 314.6, 87.2, 7.3, 202.6, 391.7,
      529.9, 783.1, 956.9, 1116.5, 1269.3, 794.3, 535.1, 1269.3, 1116.5, 956.9,
      783.1, 529.9, 391.7, 514.8, 673.4, 968.5, 1630.6, 1839.7, 2028.0, 2224.0,
      2355.7, 1837.6, 1269.3, 391.7, 202.6, 922.9, 1074.1, 1405.9, 1744.9,
      2012.3, 1630.6, 968.5, 673.4, 514.8, 391.7, 2355.7, 2224.0, 2028.0,
      1839.7, 1630.6, 2012.3, 2930.1, 3377.7, 2917.0, 2355.7, 2355.7, 2917.0,
      3377.7, 3896.5, 4343.1, 4415.2, 4429.8, 4279.0, 4113.7, 3344.4, 2790.6,
      2574.0, 2414.3, 2355.7, 535.1, 794.3, 1269.3, 1837.6, 2355.7, 2414.3,
      2574.0, 2790.6, 1920.3, 992.4, 847.9, 702.9, 535.1, 202.6, 50.8, 7.3,
      34.8, 98.8, 170.8, 533.0, 1074.1, 1405.9, 1074.1, 922.9, 202.6
    ),
    biome = c(
      rep('Subtropical desert', 9), rep('Temperate grassland/desert', 7),
      rep('Woodland/shrubland', 13), rep('Temperate forest', 16),
      rep('Boreal forest', 12), rep('Temperate rain forest', 10),
      rep('Tropical rain forest', 14), rep('Tropical seasonal forest/savanna', 13),
      rep('Tundra', 12)
    )
  )
  
  # STEP 2
  # Merge deserts if specified
  if (merge_deserts){
    
    biome <- as.character(biomes_df$biome)
    
    biome[grepl('desert', biome, fixed = TRUE)] <- 'Desert'
    
    biomes_df$biome <- as.factor(biome)
    
  }
  
  # STEP 3
  # Create SpatialPolygonsDataFrame object
  list_pol <- sapply(as.character(unique(biomes_df$biome)),
                     function(id_biome,df)
                       sp::Polygon(cbind(df$map[df$biome == id_biome],
                                         df$mat[df$biome == id_biome])),
                     df=biomes_df, USE.NAMES = TRUE)
  
  sp_biomes <- sp::SpatialPolygons(
    lapply(1:length(list_pol),
           function(i, x) {sp::Polygons(list(x[[i]]),
                                        names(x)[i])},
           x = list_pol)
  )
  
  spdf_biomes <- sp::SpatialPolygonsDataFrame(
    sp_biomes, data.frame(biome = names(list_pol)), match.ID = 'biome'
  )
  
  # STEP 4
  # Return SpatialPolygonsDataFrame object
  return(spdf_biomes)
  
  # END FUNCTION
}