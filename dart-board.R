# https://gist.github.com/CerebralMastication/278148

#draw.circle is from the plotrix library
require(plyr)

draw.circle <- function (x, y, radius, nv = 100, border = NULL, col = NA, lty = 1, 
                         lwd = 1) 
{
    xylim <- par("usr")
    plotdim <- par("pin")
    ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
    angle.inc <- 2 * pi/nv
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
    xv <- cos(angles) * radius + x
    yv <- sin(angles) * radius * ymult + y
    polygon(xv, yv, border = border, col = col, lty = lty, lwd = lwd)
    invisible(list(x = xv, y = yv))
}


drawBoard <- function(title = "Dart Board") {
    plot(-260:260, -260:260, type="n", xlab="", ylab="", asp = 1, main=title) 
    # draw.circle(0, 0, 12.7/2, border="purple", lty=1, lwd=1) #  bull
    draw.circle(0, 0, 31.8/2, border="purple", lty=1, lwd=1) #  outer bull
    # draw.circle(0, 0, 107, border="purple", lty=1, lwd=1) 
    draw.circle(0, 0, 99, border="purple", lty=1, lwd=1)  
    # draw.circle(0, 0, 162, border="purple", lty=1, lwd=1) 
    draw.circle(0, 0, 170, border="purple", lty=1, lwd=1)
    # draw.circle(0, 0, 107, border="purple", lty=1, lwd=1)
    draw.circle(0, 0, 99, border="purple", lty=1, lwd=1)
    draw.circle(0, 0, 451/2, border="black", lty=1, lwd=1) #outer edge of board
    
    angle.inc <- 2 * pi/20 # 20 sections in a board
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
    angles <- angles - .5 * pi / 10 #dart boards are rotated slightly 
    xSeries <- sin(angles) * 170
    ySeries <- cos(angles) * 170
    
    for (i in 1:20){
        lines(c(0,xSeries[i]),c(0,ySeries[i] ))
    }
    
    points <- c(20,  1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5)
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
    xSeries <- sin(angles) * 180
    ySeries <- cos(angles) * 180
    for (i in 1:20){
        text(xSeries[i],ySeries[i], points[i] )
    }
}

throw <- function(n, targetX, targetY, stdev){
    # input the x, y coord of a throw and the standard deviation of throw pattern
    # return the x,y coord of the resulting throw
    # assumes normal distribution of throws
    # returns a data frame with columns xThrow and yThrow
    
    distanceFromTarget <- rnorm(n, 0, stdev)
    angleFromTarget <-     runif(n, 0, 2 * pi)
    xOffset <- sin(angleFromTarget) * distanceFromTarget
    yOffset <- cos(angleFromTarget) * distanceFromTarget
    
    xThrow <- targetX + xOffset
    yThrow <- targetY + yOffset
    return(data.frame(xThrow, yThrow ))
}

throwNormXY <- function(n, targetX, targetY, stdevX, stdevY){
    # input the x, y coord of a throw and the standard deviation of X and Y
    # return the x,y coord of the resulting throw
    # assumes normal distribution of throws
    # returns a data frame with columns xThrow and yThrow
    
    xOffset <- rnorm(n, 0, stdevX)
    yOffset <- rnorm(n, 0, stdevY)
    
    xThrow <- targetX + xOffset
    yThrow <- targetY + yOffset
    return(data.frame(xThrow, yThrow ))
}

scoreThrow <- function(xThrow, yThrow){
    # function for turning the x,y coordinates of a throw into 
    # a point score
    angle.inc <- 2 * pi/20 # 20 sections in a board
    points <- c(20,  1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5)
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
    angles <- angles - .5 * pi / 10 #dart boards are rotated slightly 
    
    distance <- (xThrow^2 + yThrow^2)^.5
    
    angle <- (.5 * pi) - asin(yThrow/distance)
    angle <- if (xThrow < 0 ) {  pi + (pi - angle )
    } else {angle}
    
    #determine which angle section the throw was in
    angleSection <- NA
    for (i in 1:19){
        if (angles[i] < angle & angles[i+1] >= angle) {angleSection <- i} 
    } #there has to be a prettier way to do this
    if (is.na(angleSection)==TRUE) {angleSection <- 20}
    
    #determine which band the throw was in (1-6) 1 is bull, 2 is outer bull etc
    band <- if (distance <= 12.7/2) {1
    } else if (distance >= 12.7/2 & distance < 31.8/2)  {2
    } else if (distance >= 31.8/2 & distance < 99)   {3
    } else if (distance >= 99     & distance < 107)  {4
    } else if (distance >= 107    & distance < 162)  {5
    } else if (distance >= 162    & distance < 170)  {6 
    } else {7}
    
    bandScore <- c(50, 25, 1, 3, 1, 2, 0)
    finalScore <- if (band <= 2) {bandScore[band]} else {points[angleSection] * bandScore[band] }    
    return(finalScore)
}


ePoints <- function(n, xPoint, yPoint, sDev) {
    #takes in an x, y point and simultes it n times
    #returs a single value which is the average score per throw
    throws <- throw(n, xPoint, yPoint, sDev)
    scores <- ddply(throws,c("xThrow", "yThrow"), function(df) scoreThrow(df$xThrow, df$yThrow))
    names(scores) <- c("xThrow", "yThrow", "points")
    return(mean(scores$points))
}



runtest <- function(){
    drawBoard()
    require(plyr)
    yThrow <- (rep(20,15))
    yThrow <- c(yThrow, (rep(-20,15)))
    yThrow <- c(yThrow, (rep(60,15)))
    yThrow <- c(yThrow, (rep(-60,15)))
    yThrow <- c(yThrow, (rep(100,15)))
    yThrow <- c(yThrow, (rep(-100,15)))
    yThrow <- c(yThrow, (rep(140,15)))
    yThrow <- c(yThrow, (rep(-140,15)))
    
    xThrow <- rep(seq(-175,175, 25),8)
    
    throws <- data.frame(xThrow, yThrow)
    points(throws$xThrow, throws$yThrow, col="red", pch=18 )
    scores <- ddply(throws,c("xThrow", "yThrow"), function(df) scoreThrow(df$xThrow, df$yThrow))
    text(scores$xThrow, scores$yThrow, scores$V1, col="blue")
}

runtest()






layout(matrix(1:4, 2, 2))
throw_not_accurate_not_precise <- throw(10, 100, 100, 100)
drawBoard(title = "not accurate and not precise")
points(throw_not_accurate_not_precise, pch = 16, col = "orange")

throw_accurate_not_precise <- throw(10, 0, 0, 100)
drawBoard(title = "accurate and not precise")
points(throw_accurate_not_precise, pch = 16, col = "orange")

throw_not_accurate_precise <- throw(10, 100, 100, 20)
drawBoard(title = "not accurate but precise")
points(throw_not_accurate_precise, pch = 16, col = "orange")

throw_accurate_precise <- throw(10, 0, 0, 20)
drawBoard(title = "accurate and precise")
points(throw_accurate_precise, pch = 16, col = "orange")


