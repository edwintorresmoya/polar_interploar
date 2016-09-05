# polar_interploar
interpolation circular
PolarImageInterpolate_2 =function(x, y, z, outer.radius = 1,
breaks, col, nlevels = 20, contours = TRUE, legend = TRUE,
axes = TRUE, circle.rads = pretty(c(0,outer.radius))){
minitics <- seq(-outer.radius, outer.radius, length.out = 1000)
# interpolate the data
Interp <- akima:::interp(x = x, y = y, z = z,
extrap = TRUE,
xo = minitics,
yo = minitics,
linear = FALSE)
Mat <- Interp[[3]]
# mark cells outside circle as NA
markNA <- matrix(minitics, ncol = 1000, nrow = 1000)
Mat[!sqrt(markNA ^ 2 + t(markNA) ^ 2) < outer.radius] <- NA
# sort out colors and breaks:
if (!missing(breaks) & !missing(col)){
if (length(breaks) - length(col) != 1){
stop("breaks must be 1 element longer than cols")
}
}
if (missing(breaks) & !missing(col)){
breaks <- seq(min(Mat,na.rm = TRUE), max(Mat, na.rm = TRUE), length = length(col) + 1)
nlevels <- length(breaks) - 1
}
if (missing(col) & !missing(breaks)){
col <- rev(heat.colors(length(breaks) - 1))
nlevels <- length(breaks) - 1
}
if (missing(breaks) & missing(col)){
breaks <- seq(min(Mat,na.rm = TRUE), max(Mat, na.rm = TRUE), length = nlevels + 1)
col <- rev(heat.colors(nlevels))
}
# if legend desired, it goes on the right and some space is needed
if (legend) {
par(mai = c(1,1,1.5,1.5))
}
# begin plot
image(x = minitics, y = minitics, t(Mat), useRaster = TRUE, asp = 1,
axes = FALSE, xlab = "", ylab = "", col = col, breaks = breaks)
# add contours if desired
if (contours){
CL <- contourLines(x = minitics, y = minitics, t(Mat), levels = breaks)
A <- lapply(CL, function(xy){
lines(xy$x, xy$y, col = gray(.2), lwd = .5)
})
}
# add radial axes if desired
if (axes){
# internals for axis markup
RMat <- function(radians){
matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), ncol = 2)
}
circle <- function(x, y, rad = 1, nvert = 500){
rads <- seq(0,2*pi,length.out = nvert)
xcoords <- cos(rads) * rad + x
ycoords <- sin(rads) * rad + y
cbind(xcoords, ycoords)
}
# draw circles
if (missing(circle.rads)){
circle.rads <- pretty(c(0,outer.radius))
}
for (i in circle.rads){
lines(circle(0, 0, i), col = "#66666650")
}
# put on radial spoke axes:
axis.rads <- c(0, pi / 6, pi / 3, pi / 2, 2 * pi / 3, 5 * pi / 6)
r.labs <- c(12, 10, 8, 6, 4, 2)
l.labs <- c(24, 22, 20, 18, 16, 14)
for (i in 1:length(axis.rads)){
endpoints <- zapsmall(c(RMat(axis.rads[i]) %*% matrix(c(1, 0, -1, 0) * outer.radius,ncol = 2)))
segments(endpoints[1], endpoints[2], endpoints[3], endpoints[4], col = "#66666650")
endpoints <- c(RMat(axis.rads[i]) %*% matrix(c(1.1, 0, -1.1, 0) * outer.radius, ncol = 2))
lab1 <- bquote(.(r.labs[i]))
lab2 <- bquote(.(l.labs[i]))
text(endpoints[1], endpoints[2], lab1, xpd = TRUE)
text(endpoints[3], endpoints[4], lab2, xpd = TRUE)
}
axis(2, pos = -1.2 * outer.radius, at = sort(union(circle.rads,-circle.rads)), labels = NA)
text( -1.21 * outer.radius, sort(union(circle.rads, -circle.rads)),sort(union(circle.rads, -circle.rads)), xpd = TRUE, pos = 2)
}
# add legend if desired
# this could be sloppy if there are lots of breaks, and that's why it's optional.
# another option would be to use fields:::image.plot(), using only the legend.
# There's an example for how to do so in its documentation
if (legend){
ylevs <- seq(-outer.radius, outer.radius, length = nlevels + 1)
rect(1.2 * outer.radius, ylevs[1:(length(ylevs) - 1)], 1.3 * outer.radius, ylevs[2:length(ylevs)], col = col, border = NA, xpd = TRUE)
rect(1.2 * outer.radius, min(ylevs), 1.3 * outer.radius, max(ylevs), border = "#66666650", xpd = TRUE)
text(1.3 * outer.radius, ylevs,round(breaks, 1), pos = 4, xpd = TRUE)
}
}
