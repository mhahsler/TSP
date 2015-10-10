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
   
    if(is.character(cut)) cut <- which(labels(x) == cut)
    else cut <- which(x == cut) ## city id
    if(length(cut)!=1) stop("cut has to exist in the tour!")

    exclude_cut <- if(exclude_cut) 1 else 0
    
    path <- c(x,x)[(cut + exclude_cut):(length(x) + cut - 1)]
    path
}

##generic
cut_tour <- function(x, cut, exclude_cut = TRUE)
    UseMethod("cut_tour")

    
