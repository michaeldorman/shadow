.radiationGrid = function(
  grid,
  obstacles,
  obstacles_height_field,
  solar_pos,
  solar_normal,
  solar_diffuse,
  returnList,
  parallel
) {

  # Determine shadow
  cat("Determining Shadow\n")
  s = inShadow(
    location = grid,
    obstacles = obstacles,
    obstacles_height_field = obstacles_height_field,
    solar_pos = solar_pos,
    parallel = parallel
  )
  cat("\nDone\n")

  # Calculate 'direct' radiation coefficient
  coef = coefDirect(
    type = grid$type,
    facade_az = grid$facade_az,
    solar_pos = solar_pos
  )

  # Set coefficient to zero where shaded
  coef = coef * !s

  # Multiply by 'direct' radiation load
  direct = sweep(
    x = coef,
    MARGIN = 2,
    STATS = solar_normal,
    FUN = "*"
  )

  # Calculate 'SVF'
  cat("Calculating SVF\n")
  grid$svf = SVF(
    location = grid,
    obstacles = obstacles,
    obstacles_height_field = obstacles_height_field,
    parallel = parallel
  )
  cat("Done\n")

  # Calculate 'diffuse' radiation
  diffuse = outer(grid$svf, solar_diffuse)

  if(returnList) {

    result = list(
      direct = direct,
      diffuse = diffuse
    )

  } else {

    # Sum radiation
    direct_sum = rowSums(direct)
    diffuse_sum = rowSums(diffuse)
    total_sum = direct_sum + diffuse_sum

    # Return result
    result = data.frame(
      svf = grid$svf,
      direct = direct_sum,
      diffuse = diffuse_sum,
      total = total_sum
    )

  }

  return(result)

}
