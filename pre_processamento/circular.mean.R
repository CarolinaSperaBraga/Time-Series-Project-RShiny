circular.mean <- function(angles){
  angles_pi <- (angles) * pi / 180
  C <- sum(cos(angles_pi))
  S <- sum(sin(angles_pi))
  mean_pi <- atan(S/C)
  if(S > 0 & C > 0){
    mean <- mean_pi * 180 / pi  
  } else if(C < 0) {
    mean <- (mean_pi + pi) * 180 / pi
  } else {
    mean <- (mean_pi + 2*pi) * 180 / pi
  }
  return(mean)
}
