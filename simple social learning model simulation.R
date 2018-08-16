# simple social learning model simulation

sim <- function( tmax=500 , b=2 , c=1 , u=0.1 , w0=10 , p1=0.01 ) {
	p <- rep( NA , tmax )
	q <- rep( NA , tmax )
	p[1] <- p1
	q[1] <- 0
	for ( t in 2:tmax ) {
		
		ut <- rbinom(1,size=1,prob=u)
		q[t] <- (1-ut)*( 1-p[t-1] + p[t-1]*q[t-1] ) + ut*0
		
		WI <- w0 + b - c
		WS <- w0 + b*q[t]

		p[t] <- p[t-1]*WS / ( p[t-1]*WS + (1-p[t-1])*WI )
	}
	return(
		list(
			p=p,
			q=q,
			b=b,
			c=c,
			u=u,
			w0=w0,
			p1=p1,
			tmax=tmax
		)
	)
}

x <- sim( u=0.1 , tmax=2000 , w0=0.1 )

plot( x$p , type="l" , lwd=2 )
lines( 1:x$tmax , x$q , col="blue" , lwd=2 )

abline( h=mean(x$p[-(1:200)]) , lty=2 ) 
abline( h=mean(x$q[-(1:200)]) , lty=2 , col="blue" )

abline( h= with( x , (1-u*b/c)/(1-u) ) )
abline( h= with( x , 1-c/b ) , col="blue" )
