GenerateFiberGrid <- function(grid_x, grid_y, grid_z) {

  grid <- array(0, dim=c(grid_x, grid_y, grid_z))
  grid.coord.mat <- expand.grid(1:grid_x, 1:grid_y, 1:grid_z)
  SelectNextPoint(grid.coord.mat, c(0, 0, 0), 10)

}

SelectNextPoint <- function(grid.coord.mat, current.point, radius) {
  dist.vec <- apply(grid.coord.mat, 1, function(x, current.point){return(CalculateDistance(x, current.point))}, current.point)
  browser()
}

CalculateDistance <- function(point1, point2) {
  return(sqrt(sum((point1 - point2)^2)))
}
