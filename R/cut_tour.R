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

    ## city label
    if(is.character(cut)) {
        cut <- which(as.logical(apply(sapply(cut, "==", labels(x)), MARGIN = 1, sum)))
        if(length(cut)<1) stop("cut has to exist")

    ## city id
    } else {
        if(any(is.na(cut <- match(cut, x)))) stop("cut has to exist")
    }

    if(length(cut) == 1L) { ## single path
        path <- c(x,x)[(cut + as.numeric(exclude_cut)):(length(x) + cut - 1L)]

    } else { ## multiple paths, return as a list

        path_names <- labels(x)[cut]

        ## make first cut the begining. Note we keeb the boundary at the begining and the end!
        path <- c(x,x)[cut[1]:(length(x) + cut[1])]
        cut2 <- c(cut - cut[1] + 1L, length(x))

        path <- lapply(2:length(cut2), FUN =
                function(i) path[(cut2[i-1L] + as.numeric(exclude_cut)):(cut2[i]-1L)])

        names(path) <- path_names
    }

    path
}

##generic
cut_tour <- function(x, cut, exclude_cut = TRUE)
    UseMethod("cut_tour")
