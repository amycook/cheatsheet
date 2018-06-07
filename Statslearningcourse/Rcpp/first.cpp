#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double f1(NumericVector x) {
        int n = x.size();
        double y = 0;
        
        for(int i = 0; i < n; ++i) {
                y += x[i] / n;
        }
        return y;
}

// [[Rcpp::export]]

NumericVector f5(NumericVector x, NumericVector y) {
        int n = std::max(x.size(), y.size());
        NumericVector x1 = rep_len(x, n);
        NumericVector y1 = rep_len(y, n);
        
        NumericVector out(n);
        
        for (int i = 0; i < n; ++i) {
                out[i] = std::min(x1[i], y1[i]);
        }
        
        return out;
}


// [[Rcpp::export]]

bool all(LogicalVector x) {
        int n = x.size();
        
        for (int i = 0; i < n; ++i) {
                if(x[i]) return true;
        }
        
        return false;
}

// [[Rcpp::export]]

NumericVector cumprodC(NumericVector x) {
        int n = x.size();
        NumericVector total(n);
        total[0] = x[0];
        
        for (int i = 1; i < n; ++i) {
                total[i] = total[i-1]*x[i];
        }
        
        return total;
}

// [[Rcpp::export]]

double varC(NumericVector x) {
        int n = x.size();
        double sum_diffs = 0;
        double mean_t = 0;
        
        for (int i = 0; i < n; ++i) {
                mean_t += x[i]/n;
        }
        
        for (int i = 0; i < n; ++i) {
                sum_diffs += pow(x[i]-mean_t, 2);
        }
        
        return sum_diffs/n;
}

// [[Rcpp::export]]
NumericVector attribs() {
        NumericVector out = NumericVector::create(1, 2, 3);
        
        out.names() = CharacterVector::create("a", "b", "c");
        out.attr("my-attr") = "my-value";
        out.attr("class") = "my-class";
        
        return out;
}


// input a list - ie results from an lm()
// extract mean percentage error from the lm() results
// [[Rcpp::export]]

double mpe(List mod) {
        if (!mod.inherits("lm")) stop("Input must be a linear model");
        
        NumericVector resid = as<NumericVector>(mod["residuals"]);
        NumericVector fitted = as<NumericVector>(mod["fitted.values"]);
        
        int n = resid.size();
        double err = 0;
        for(int i = 0; i < n; ++i) {
                err += resid[i] / (fitted[i] + resid[i]);
        }
        return err / n;
}

// Put R functions in an object of type Function. Call R functions from C++
// returns an 'RObject' because we do not know what type of object an R function will return
// [[Rcpp::export]]

RObject callWithOne(Function f) {
        return f(1);
}

/*** R
# callWithOne(function(x) x + 5)
# callWithOne(as.list)
# callWithOne(paste)
        
*/


// [[Rcpp::export]]
List scalar_missings() {
        int int_s = NA_INTEGER;
        String chr_s = NA_STRING;
        bool lgl_s = NA_LOGICAL;
        double num_s = NA_REAL;
        
        return List::create(int_s, chr_s, lgl_s, num_s);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# all(c(TRUE == TRUE, is.numeric(10)))
# cumprodC(c(1,2,3,4))

# scalar_missings()

##
# missing values
##
# in integers, missing values are stored as the smallest integer
# missing values (NA) in integers can go really wrong. either use length 1 IntegerVector or be careful
# with doubles may be able to use NaN instead of NA. R's NA is a floating point NaN??
# things go weird with NaN in boolean expressions because there are only two options T or F
        # whereas in R there are 3: T, F, NA
        # if you coerce a logical vector in C++, be careful because NA's will become TRUE
# NaNs are good at returning NaN in numeric expresssions, ie NaN - 1 = NaN
# strings and NaN are fine
# NA seems to work a lot better within vectors - of any type Numeric, Integer, Logical, Character

Rcpp::evalCpp('NA_INTEGER + 1')

*/




