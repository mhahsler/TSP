#######################################################################
# TSP - Traveling Salesperson Problem
# Copyrigth (C) 2011 Michael Hahsler and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#' Cut a tour to form a path
#'
#' Cuts a tour at a specified city to form a path.
#'
#' @family TOUR
#'
#' @param x an object of class [TOUR].
#' @param cut the index or label of the city/cities to cut the tour.
#' @param exclude_cut exclude the city where we cut? If `FALSE`, the city
#' at the cut is included in the path as the first city.
#' @return Returns a named vector with city ids forming the path. If multiple
#' cuts are used then a list with paths is returned.
#' @author Michael Hahsler
#' @keywords optimize
#' @examples
#'
#' data("USCA50")
#'
#' ## find a path starting at Austin, TX
#' tour <- solve_TSP(USCA50)
#' path <- cut_tour(tour, cut = "Austin, TX", exclude_cut = FALSE)
#' path
#'
#' ## cut the tours at two cities
#' tour <- solve_TSP(USCA50)
#' path <- cut_tour(tour, cut = c("Austin, TX", "Cambridge, MA"), exclude_cut = FALSE)
#' path
#'
#' ## cut a tour at the largest gap using a dummy city
#' tsp <- insert_dummy(USCA50, label = "cut")
#' tour <- solve_TSP(tsp)
#'
#' ## cut tour into path at the dummy city
#' path <- cut_tour(tour, "cut")
#' path
#' @export
cut_tour <- function(x, cut, exclude_cut = TRUE)
    UseMethod("cut_tour")

#' @rdname cut_tour
#' @export
cut_tour.TOUR <- function(x, cut, exclude_cut = TRUE) {

    exclude_cut <- as.integer(exclude_cut)

    ## city label
    if(is.character(cut)) {
        cut <- which(as.logical(apply(sapply(cut, "==", labels(x)), MARGIN = 1, sum)))
        if(length(cut) < 1) stop("cut has to exist")

    ## city id
    } else {
        if(any(is.na(cut <- match(cut, x)))) stop("cut has to exist")
    }

    if(length(cut) == 1L) { ## single path
        if (exclude_cut && length(x) <= 1)
            path <- integer(0)
        else
            path <- c(x, x)[(cut + exclude_cut):(length(x) + cut - 1L)]

    } else { ## multiple paths, return as a list
        path <- replicate(length(cut), integer(0))
        path[[1L]] <- c(
            if ((tail(cut, 1) + exclude_cut) <= length(x)) (tail(cut, 1) + exclude_cut):length(x) else NULL,
            if (cut[1] > 1) 1:(cut[1] - 1L) else NULL)
        for (i in seq_len(length(cut) - 1L)) {
            if ((cut[i] + exclude_cut) <= (cut[i + 1L] - 1L))
                path[[i + 1L]] <- (cut[i] + exclude_cut):(cut[i + 1L] - 1L)
        }

        path <- lapply(path, FUN = function(i) x[i])
        if (exclude_cut) names(path) <- labels(x)[cut]
    }

    path
}
