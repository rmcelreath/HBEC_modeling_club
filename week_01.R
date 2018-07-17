# define recursion function
pprime <- function( p , VA=0.8 , VB=0.5 ) p*VA / ( p*VA + (1-p)*VB )

# iterate
( pp <- pprime( 0.1 ) )
# repeat the following line to iterate
( pp <- pprime( pp ) )

# now plot the time trend
p <- rep(NA,50)
p[1] <- 0.01
for ( i in 2:50 ) p[i] <- pprime( p[i-1] )
plot( p , type="b" )

# now plot the change in p in each generation
Dp <- rep(NA,50)
for ( i in 2:50 ) Dp[i] <- p[i] - p[i-1]
plot( Dp , type="b" )
