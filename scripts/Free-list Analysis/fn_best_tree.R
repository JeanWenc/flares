#######################################################################################################################
###################FLARES - Free-List Analysis under R Environment using Shiny########################################
#######################################################################################################################
#Copyright (C) 2017 Jean Wencélius

#License notice:
#This file is part of FLARES.

#FLARES is a free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License
#along with FLARES.  If not, see <http://www.gnu.org/licenses/>.

########################################################################################################################

best.cutree <- function(hc, min, max, loss=FALSE, graph=FALSE){
  hc<-hc
  max <- min(max, length(hc$height))
  inert.gain <- rev(hc$height)
  intra <- rev(cumsum(rev(inert.gain)))
  relative.loss = intra[min:(max)]/intra[(min - 1):(max - 1)]
  best = which.min(relative.loss)
  names(relative.loss) <- min:max
  best.n<-as.numeric(names(relative.loss)[best])
  if (graph) {
    temp <- relative.loss
    temp[best] <- NA
    best2 <- which.min(temp)
    pch <- rep(1, max-min+1)
    pch[best] <- 16
    pch[best2] <- 21
    plot(min:max, relative.loss, pch=pch, bg="grey75")
  } else {
    if (loss)
      relative.loss
    else
      best + min - 1
  }
  return(best.n)
}