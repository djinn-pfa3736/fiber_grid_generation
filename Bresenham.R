Bresenham <- function(x1, y1, z1, x2, y2, z2, size.x, size.y, size.z) {
  dx <- abs(x2 - x1)
  dy <- abs(y2 - y1)
  dz <- abs(z2 - z1)

  if(x1 < x2) {
    sx <- 1
  } else {
    sx <- -1
  }

  if(y1 < y2) {
    sy <- 1
  } else {
    sy <- -1
  }

  if(z1 < z2) {
    sz <- 1
  } else {
    sz <- -1
  }

  error.xy <- dx - dy
  error.yz <- dy - dz
  grid <- array(0, dim = c(size.x, size.y, size.z))
  while(TRUE) {
    grid[x1, y1, z1] <- 1
    if(x1 == x2 && y1 == y2 && z1 == z2) break
    e2.xy <- 2*error.xy
    e2.yz <- 2*error.yz
    if(e2.xy > -dy) {
      error.xy <- error.xy - dy
      x1 <- x1 + sx
    }
    if(e2.xy < dx) {
      error.xy <- error.xy + dx
      y1 <- y1 + sy
    }

    if(e2.yz > -dz) {
      error.yz <- error.yz - dz
      y1 <- y1 + sx
    }
    if(e2.yz < dy) {
      error.yz <- error.yz + dy
      z1 <- z1 + sz
    }

  }

  return(grid)
}
