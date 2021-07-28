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

##generic
cut_tour <- function(x, cut, exclude_cut = TRUE)
    UseMethod("cut_tour")
