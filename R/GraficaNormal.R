media <- 41
desviacion <- 6
valor <- 47
curve(dnorm(x, media, desviacion), xlim = c(media - 3 * desviacion, media + 3 * desviacion), xlab="", ylab="", add=F, type="l", col=4, lwd=5, bty="n", yaxt='n', xaxt='n')

cord.x = c(media - 3 * desviacion, seq(media - 3 * desviacion, valor, 0.001), valor)
cord.y = c(0, dnorm(seq(media - 3 * desviacion, valor, 0.001), media, desviacion), 0)
polygon(cord.x, cord.y, col='gray', border="red", lwd=2)

cord.x = c(media - 3 * desviacion, seq(media - 3 * desviacion, media, 0.001), media)
cord.y = c(0, dnorm(seq(media - 3 * desviacion, media, 0.001), media,desviacion), 0)
polygon(cord.x, cord.y, col=rgb(255, 255, 255, max = 255, alpha = 0), border="black", lwd=2)
cord.x = c(media, seq(media, media + 3 * desviacion, 0.001), media + 3 * desviacion)
cord.y = c(0, dnorm(seq(media, media + 3 * desviacion, 0.001), media, desviacion), 0)
polygon(cord.x, cord.y, col=rgb(255, 255, 255, max = 255, alpha = 0), border="black", lwd=2)

cord.y = rep(dnorm(41,media-desviacion,desviacion),4)
cord.x = c(media-desviacion,media-desviacion,media,media)
#polygon(cord.x, cord.y, col=rgb(255, 255, 255, max = 255, alpha = 0), border="black", lwd=2)


curve(dnorm(x, media, desviacion), xlim = c(media - 3 * desviacion, media + 3 * desviacion), xlab="", ylab="", add=T, type="l", col=4, lwd=5, bty="n", yaxt='n', xaxt='n')
axis(1, at=seq(media - 3 * desviacion, media + 3 * desviacion, by=2), labels = FALSE)
lablist = as.vector(seq(media - 3 * desviacion, media + 3 * desviacion,2))
text(x = media, y = 0, labels = media, adj = c(0, 3), xpd=T)
text(x = valor, y = 0, labels = valor, adj = c(0, 3), xpd=T)
pnorm(valor, mean=media, sd=desviacion, lower.tail=T) 

