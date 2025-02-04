#This function calculates a running slope along a curve
#between consecutive points
consec.slope <- function(x, y){

    consec.x <- consec_pairs(x)
    consec.y <- consec_pairs(y)

    all.slopes <- rep(NA, nrow(consec.x))
    for(i in 1:nrow(consec.x)){
        all.slopes[i] <- get.slope(consec.x[i,1], consec.x[i,2], consec.y[i,1], consec.y[i,2])
    }

    return(all.slopes)
}