library(Rcpp)
#> 
#> Attaching package: 'Rcpp'
#> 
#> The following object is masked from 'package:inline':
#> 
#>     registerPlugin
cppFunction('int add(int x, int y, int z) {
        int sum = x + y + z;
        return sum;
        }')
# add works like a regular R function
add
#> function (x, y, z) 
#> .Primitive(".Call")(<pointer: 0x7f2f4aa933d0>, x, y, z)
add(1, 2, 3)
#> [1] 6


# function without any inputs
cppFunction('int one() {
return 1;
}')
one()


cppFunction('int SignC(int x){
            if(x>0) {
                return 1;
            } else if(x == 0) {
                return 0;
            } else {
                return -1;
            }
            
            }')
SignC(5)
SignC(-3)


#for loops carry little overhead in C++

cppFunction('double sumC(NumericVector x){
            int n = x.size();
            double total = 0;
            for(int i = 0; i < n; ++i){
                total += x[i];
            }
            return total;
            }
            ')
sumC(c(1:100))

# function for euclidean distance

cppFunction('NumericVector pdistC(double x, NumericVector ys){
            int n = ys.size();
            NumericVector dist(n);
            
            for(int i = 0; i<n; ++i){
                dist[i] = sqrt(pow(x-ys[i], 2));
            }

            return dist;
            }
            ')

#rowsums in C
# start with a matrix. output a vector for each row

cppFunction('NumericVector rowSumsC(NumericMatrix x){
            int nrow = x.nrow();
            int ncol = x.ncol();
            NumericVector sums(nrow);
            
            for(int i = 0; i< nrow; ++i){

                for(int j = 0; j < ncol; ++j){
                        sums[i] += x(i,j);
                }

            }

            return sums;
            }
            ')


test = matrix(data = c(1:4), nrow = 2, ncol = 2)
rowSumsC(test)
















