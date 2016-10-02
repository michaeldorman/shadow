classifyAz = function(sl) {

  # For each feature
  for(i in 1:length(sl)) {

    # Select one
    s = sl[i, ]
    m = s@lines[[1]]@Lines[[1]]@coords

    # Calculate segment direction
    x1 = m[1, 1]
    y1 = m[1, 2]
    x2 = m[2, 1]
    y2 = m[2, 2]

    if(x2 >= x1 & y2 > y1) {az = 360 - (180 / pi) * atan((y2-y1) / (x2-x1)); q = 1}
    if(x2 >= x1 & y2 <= y1) {az = (180 / pi) * atan((y1-y2) / (x2-x1)); q = 2}
    if(x2 < x1 & y2 <= y1) {az = 180 - (180 / pi) * atan((y1-y2) / (x1-x2)); q = 3}
    if(x2 < x1 & y2 > y1) {az = 180 + (180 / pi) * atan((y2-y1) / (x1-x2)); q = 4}

    sl$az[i] = az
    sl$q[i] = q

  }

}
