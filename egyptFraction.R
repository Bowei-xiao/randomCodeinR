# Function to calculate the Egyptian Fraction for any rational fraction <1
# rational fraction here merely meaning that fraction is representing a rational number
gcd = function(x,y) {
# Helper Function get the greatest common divisor, assuming x>=y  
  r = x%%y;
  return(ifelse(r, gcd(y, r), y))
}

egyFrac = function(a,b,lane=NULL){
# Main function. a is the numerator, and b is the denominator
# assume a, b be integer and a<b
# Example: 2/5 -> egyFrac(2,5)
  if (a==1){
    lane = paste0(lane,'1/',b)
    return(lane)
  } else{
    a_new = (-b %% a)
    b_new = b*(as.integer(b/a)+1)
    lane_new = paste0(lane,'1/',(as.integer(b/a)+1),'+')
    cd = gcd(b_new,a_new)
    if (cd != 1){
      b_new =b_new / cd
      a_new =a_new / cd
      }
    egyFrac(a=a_new,b=b_new,lane=lane_new)
  }
}
