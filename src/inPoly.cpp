// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

//' Check if a point is contained within a polygon
//'
//' @param point A \code{rowvec} with x,y coordinate structure.
//' @param vertices A \code{matrix} containing the boundary points of the polygon.
//' @return A \code{bool} indicating whether the point is in the polygon (TRUE) or not (FALSE)
// [[Rcpp::export(pointInPoly)]]
bool pointInPoly(const arma::rowvec& point, const arma::mat& vertices) {
  unsigned int i, j;
  double x = point(0), y = point(1);
  bool inside = false;
  for (i = 0, j = vertices.n_rows - 1; i < vertices.n_rows; j = i++) {
    double xi = vertices(i,0), yi = vertices(i,1);
    double xj = vertices(j,0), yj = vertices(j,1);
    // if point is a vertex, it's inside:
    if((x == xi && y == yi) || (x == xj && y == yj)) { // this may be redundant now that we check edges.. but maybe still more efficient to check vertices first
      return true;
    }
    // if point is on an edge, it's inside.
    // First, check whether it's on vertical/horizontal edges (and within range of edge):
    if(
      ((x == xi) && (x == xj) && ((yi <= y) != (yj <= y))) ||
        ((y == yi) && (y == yj) && ((xi <= x) != (xj <= x)))
    ) {
      return true;
    }
    // If not, check whether it lies within the x and y- range of the edge..
    //if(((xi <= x) && (x <= xj) && (yi <= y) && (y <= yj)) || ((xi >= x) && (x >= xj) && (yi >= y) && (y >= yj))) { // <- old, incorrect implementation
    if(((xi <= x) != (xj <= x)) && ((yi <= y) != (yj <= y))) {
      // and if so, whether it it on a sloped edge:
      // https://stackoverflow.com/questions/17692922/check-is-a-point-x-y-is-between-two-points-drawn-on-a-straight-line#comment61233658_17693189
      if((xi - xj) * (y - yi) == (yi - yj) * (x - xi)) {
        return true;
      }
    }
    inside ^= (((yi >= y) != (yj >= y)) && (x <= (xj - xi) * (y - yi) / (yj - yi) + xi));
  }
  return inside;
}


//' Check if multiple points are contained within a polygon
//'
//' @param points A \code{matrix} with x,y coordinate structure.
//' @param vertices A \code{field} of type {matrix} that contains the polygon coordinates to test against.
//' @return A logical \code{vector} indicating whether each point is within the polygon.
// [[Rcpp::export(pointsInPoly)]]
std::vector<bool> pointsInPoly(const arma::mat& points, const arma::mat& vertices) {
  unsigned int i;
  unsigned int n = points.n_rows;
  std::vector<bool> in_poly(n);
  for(i = 0; i < n; i++){
    arma::rowvec row_i = points.row(i);
    in_poly[i] = pointInPoly(row_i, vertices);
  }
  return in_poly;
}
