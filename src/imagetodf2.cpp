
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <Rcpp.h>
using namespace Rcpp; 
using namespace RcppParallel;
using namespace std;


struct pointinboxworker : public Worker  {
  
  // source vectors and matrix
  const RVector<double> x;
  const RVector<double> y;
  const RMatrix<double> V;
  int n_rows;
  
  // output vector to write to
  RVector<int> pos;
  
  // initialize from Rcpp input and output matrixes (the RMatrix class
  // can be automatically converted to from the Rcpp matrix type)
  pointinboxworker(const NumericVector x, const NumericVector y, const NumericMatrix V,  int n_rows, IntegerVector pos)
    : x(x), y(y), V(V), n_rows(n_rows), pos(pos) {}
  
  // function call operator that work for the specified range (begin/end)
  void operator()(size_t begin, size_t end) {
    for(size_t i = begin; i<end; i++){
      float xr=x[i];
      float yr=y[i]; 
      int  cn(0);    
          if (xr>V(0, 0) && xr<V(1, 0) &&yr>V(0, 1) && yr<V(2, 1)) {
            ++cn; 
      }
      
      pos[i] = cn;
    }
  }
  
};


IntegerVector pointinbox(NumericVector x,NumericVector y, NumericMatrix V) {
  
  int n_rows = V.nrow();
  
  // allocate the vector we will return
  IntegerVector pos(x.size());
  
  // create the worker
  pointinboxworker pointinboxworker(x, y, V, n_rows, pos);
  
  
  // call it with parallelFor
  parallelFor(0, x.size(), pointinboxworker);
  
  return pos;
}


//[[Rcpp::export]]

DataFrame imagetodf2(NumericVector &image_matrix, NumericMatrix V,  float bottom=0, float top=1, float left = 0, float right = 1) {  
  std::vector<double> image(image_matrix.begin(),image_matrix.end()); 
  IntegerVector color_matrix_dims(image_matrix.attr("dim"));
  int num_rows = color_matrix_dims[0];
  int num_cols = color_matrix_dims[1];
  int nrc=num_rows*num_cols;
  NumericVector  X(nrc);
  NumericVector  Y(nrc);
  
  float xm=(right-left)/num_cols;
  float ym=(top-bottom)/num_rows;
  
  for (unsigned int i=num_cols; i--;) {
    for (unsigned int j=num_rows; j--;) {
      int n(i*num_rows+j);
      X[n]=left+i*xm;
      Y[n]=top-j*ym;
    }
  }
  
  
  IntegerVector pos=pointinbox(X, Y,V);
  
  NumericVector  r(nrc);
  NumericVector  g(nrc);
  NumericVector  b(nrc);
  NumericVector  a(nrc, 1.0);
 
  std::copy(image.begin(), image.begin() + nrc-1, r.begin());
  std::copy(image.begin()+ nrc, image.begin()+nrc+nrc-1, g.begin());
  std::copy(image.begin()+nrc+nrc, image.begin()+nrc+nrc+nrc-1, b.begin());
  
  if (color_matrix_dims[1]>3){
    std::copy(image.begin()+nrc+nrc+nrc, image.end(), a.begin());
  } 
  
  return DataFrame::create(_["X"]= X, _["Y"]= Y, _["r"]= r, _["g"]= g, _["b"]= b, _["a"]= a, _["pos"]= pos) ;
}

