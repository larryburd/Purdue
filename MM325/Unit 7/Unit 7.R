coefOfDeterm <- function(x, y) {
  r <- cor(x, y)
  return(r * r)
}

lmPrediction <- function(b.0, b.1, x) {
  return(b.0 + b.1 * x)
}

lmPrediction(429.07, -1.587, 50)


t <- c(48,50,52,62,63)
g <- c(359,346,342,338,324)
coefOfDeterm(t,g)
lm(g~t)

	g.1 <- c(50,	
	91,	
	55,	
	58,	
	79,	
	70,	
	97,	
	51,	
	51,	
	59,	
	69,	
	56,	
	56,	
	41,	
	49,	
	58,	
	58,	
	99,	
	87,	
	95,	
	97)	

g.2 <- c(	70,
         	58,
         	74,
         	65,
         	60,
         	60,
         	48,
         	73,
         	73,
         	66,
         	67,
         	70,
         	73,
         	76,
         	72,
         	68,
         	73,
         	53,
         	58,
         	50,
         	55)
lm(g.2~g.1)
