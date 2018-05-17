#-----------------------------------------------------------------------------
# Purpose:  Utility functions related to geometry
# Author:   Feiyang Niu
# Date:     August 11, 2016
#-----------------------------------------------------------------------------


distance_two_points <- function(point_1, point_2) {
    # point_1 = c(x1, y1)
    # point_2 = c(x2, y2)
    return(sqrt(sum((point_1 - point_2)^2)))
}


distance_point_segment <- function(point, start, end) {
    ## point is the point to test.
    ## start -> end is the line segment to check distance.
    ##
    ## Returns distance from the line, or if the intersecting point on the line nearest
    ## the point tested is outside the endpoints of the line, the distance to the
    ## nearest endpoint.
    ##
    ## Returns 9999 on 0 denominator conditions.
    x1 <- start[1]; y1 <- start[2]
    x2 <- end[1]; y2 <- end[2]
    px <- point[1]; py <- point[2]
    ans <- NULL
    ix <- iy <- 0   # intersecting point
    # print(paste('start:', paste(start, collapse = ',')))
    # print(paste('end:', paste(end, collapse = ',')))
    lineMag <- distance_two_points(start, end)
    # print(paste('lineMag:', lineMag))
    if(lineMag < 0.00000001) {
        return(distance_two_points(point, start))
    }
    u <- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1)))
    u <- u / (lineMag * lineMag)
    if((u < 0.00001) || (u > 1)) {
        ## closest point does not fall within the line segment,
        ## take the shorter distance to an endpoint
        ix <- distance_two_points(point, start)
        iy <- distance_two_points(point, end)
        if(ix > iy)  ans <- iy
        else ans <- ix
    } else {
        ## Intersecting point is on the line, use the formula
        ix <- x1 + u * (x2 - x1)
        iy <- y1 + u * (y2 - y1)
        ans <- distance_two_points(point, c(ix, iy))
    }
    return(ans)
}


# calculatae the minimum distance between a point and a line
distance_point_line <- function(point, slope, intercept) {
    ## point = c(px, py) is the point to test.
    ## slope, intercept is the line to check distance.
    ##
    ## Returns distance from the line.
    x1 <- point[1] - 10
    x2 <- point[1] + 10
    y1 <- x1 * slope + intercept
    y2 <- x2 * slope + intercept
    distance_point_segment(point, c(x1, y1), c(x2, y2))
}


# calculate the minimum distance between a point and a spline
distance_point_polyline <- function(point, vec_x, vec_y) {
    filter_cond <- !is.na(vec_x) & !is.na(vec_y)
    vec_x <- vec_x[filter_cond]
    vec_y <- vec_y[filter_cond]
    length_x <- length(vec_x)
    length_y <- length(vec_y)
    if(length_x != length_y)
        stop('vec_x and vec_y must have the same length!')
    if(length_x == 0) {
        warning('both vec_x and vec_y must have at least one elements!')
        return(Inf)
    }
    if(length_x == 1)
        return(distance_two_points(point, c(unlist(vec_x), unlist(vec_y))))
    list_points <- as.list(data.frame(t(cbind(vec_x, vec_y))))
    start_list <- list_points[1:(length_x - 1)]
    end_list <- list_points[2:length_x]
    dists <- mapply(distance_point_segment, start = start_list, end = end_list,
                    MoreArgs = list(point = point))
    return(min(dists))
}






























