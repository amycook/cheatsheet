#lambda
lam= 0.6
#observed values in each category
defects<- c(0,1,2,3)
obs<- c(61,24,10,5)
N=100
#use poisson formula to calculate expected values in each category with mean 0.6
expected<- exp(-lam)*lam^defects/factorial(defects)*N
#adjust last value for  all values greater than 3
expected[length(defects)] = N* ppois(2,0.6, lower.tail=FALSE)
#calc difference in expect-obs
chi.vec<- (obs-expected)^2/expected
#sum the differences for final p value that can be substituted into pchisq()
chi.val<- sum(chi.val)
#p test - 2 degrees of freedom
pchisq(chi.val,2, lower.tail=FALSE)

# p value is below 5%, therefore reject poisson distribution hypothesis


