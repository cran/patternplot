// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <Rcpp.h>
using namespace Rcpp; 
using namespace RcppParallel;
using namespace std;


struct pointinpieworker : public Worker  {
  
  // source vectors and matrix
  const RVector<double> x;
  const RVector<double> y;
  const RMatrix<double> V;
  float r1; 
  float r2; 
  int n_rows;
  
  // output vector to write to
  RVector<int> pos;
  
  // initialize from Rcpp input and output matrixes (the RMatrix class
  // can be automatically converted to from the Rcpp matrix type)
  pointinpieworker(const NumericVector x, const NumericVector y,  const NumericMatrix V, float r1, float r2 ,  int n_rows, IntegerVector pos)
    : x(x), y(y), V(V), r1(r1), r2(r2),n_rows(n_rows), pos(pos) {}
  
  // function call operator that work for the specified range (begin/end)
  void operator()(size_t begin, size_t end) {
    for(size_t i = begin; i<end; i++){
      
      float xr=x[i];
      float yr=y[i]; 
      int  cn(0);
      int end=n_rows-1;
      float islefts=(V(1, 0) - V(0, 0)) * (yr - V(0, 1))/(V(1, 1) - V(0, 1))- (xr -  V(0, 0));
      float islefte=(V(end, 0) - V(end-1, 0)) * (yr - V(end-1, 1))/(V(end, 1) - V(end-1, 1))- (xr -  V(end-1, 0));  
      int a=(islefts<0&&islefte<0)?1:2;
      int b=(islefts>0&&islefte>0&&(V(end-1, 1)*V(1, 1)>0))?1:2;
      int c=(a==1||b==1)?1:2;
      int d1=((V(end-1, 1)*V(1, 1)<0)&&islefts<0&&islefte>0&&V(1, 0)>=0&&V(1, 0)+V(end-1, 0)<0)?1:2;
      int d2=((V(end-1, 1)*V(1, 1)<0)&&islefts>0&&islefte<0&&V(1, 0)>=0&&V(1, 0)+V(end-1, 0)<0)?1:2;
      int d3=((V(end-1, 1)*V(1, 1)<0)&&islefts<0&&islefte>0&&V(1, 0)<0&&V(1, 0)+V(end-1, 0)<0)?1:2;
      int d4=((V(end-1, 1)*V(1, 1)<0)&&islefts>0&&islefte<0&&V(1, 0)<0&&V(1, 0)+V(end-1, 0)<0)?1:2;
      int e=(c==1||d1==1||d2==1||d3==1||d4==1)?1:2; 
      switch(e){
      case 1:
        break;
      default:
        if (xr*xr+yr*yr<=r1*r1 && xr*xr+yr*yr>=r2*r2){
          ++cn;
        }
        
      }
      
      pos[i] = cn;
    }
  }
  
};


IntegerVector pointinpie(NumericVector x,NumericVector y, NumericMatrix V, float r1, float r2) {
  
  int n_rows = V.nrow();
  
  // allocate the vector we will return
  IntegerVector pos(x.size());
  
  // create the worker
  pointinpieworker pointinpieworker(x, y, V, r1, r2, n_rows, pos);
  
  
  // call it with parallelFor
  parallelFor(0, x.size(), pointinpieworker);
  
  return pos;
}


//[[Rcpp::export]]

DataFrame imagetodf3(NumericVector &image_matrix, NumericMatrix V,  float r1=1, float r2=0,  float bottom=0, float top=1, float left = 0, float right = 1) {  
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
  
  
  IntegerVector pos=pointinpie(X, Y,V, r1, r2);
  
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
