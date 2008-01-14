`selectmap` <-
function(var1,var2,obs,Xpoly,Ypoly,method="")
{

####################################################
# Sélection d'un point
####################################################

if (method == "point")
{
    diff<-abs(var1 - as.numeric(Xpoly)) * (max(var2) - min(var2)) + abs(var2 - as.numeric(Ypoly)) * (max(var1) - min(var1));
    if(min(diff[diff==min(diff)]/((max(var2)-min(var2)) * (max(var1) - min(var1))))<0.01)
    {
        if (length(obs) == length(var1))
        {
            obs[diff==min(diff)] <- !obs[diff==min(diff)];  
        }
        else
        {
            obs[diff==min(diff),] <- !obs[diff==min(diff),];    
        }
    }
    return(obs);
  }

####################################################
# Sélection d'un polygone
####################################################

 if (method == "poly") {
        for (i in 1:length(var1)) {
            InterNb <- 0
            for (j in 1:(length(Xpoly) - 1)) {
                X0 <- as.numeric(Xpoly[j])
                X1 <- as.numeric(Xpoly[j + 1])
                Y0 <- as.numeric(Ypoly[j])
                Y1 <- as.numeric(Ypoly[j + 1])
                if (X1 != X0) {
                  if (Y1 < Y0) {
                    A <- (Y0 - Y1)/(X0 - X1)
                    B <- Y0 - A * X0
                  }
                  else {
                    A <- (Y1 - Y0)/(X1 - X0)
                    B <- Y0 - A * X0
                  }
                  YTemp <- var1[i] * A + B
                }
                else {
                  YTemp <- var2[i]
                }
                if (Y0 < Y1) {
                  if ((Y0 < YTemp) && (YTemp < Y1) && (YTemp > 
                    var2[i])) {
                    InterNb <- InterNb + 1
                  }
                }
                if (Y1 <= Y0) {
                  if ((Y1 < YTemp) && (YTemp < Y0) && (YTemp > 
                    var2[i])) {
                    InterNb <- InterNb + 1
                  }
                }
                if (Y1 == Y0) {
                  if ((X1 < var1[i]) && (var1[i] < X0) && (YTemp > 
                    var2[i])) {
                    InterNb <- InterNb + 1
                  }
                  if ((X1 > var1[i]) && (var1[i] > X0) && (YTemp > 
                    var2[i])) {
                    InterNb <- InterNb + 1
                  }
                }
            }
            if (InterNb%%2 == 1) {
                if (length(obs) == length(var1)) {
                  if (obs[i] == FALSE) {
                    obs[i] <- !obs[i]
                  }
                }
                else {
                  if (obs[i, 1] == FALSE) {
                    obs[i, ] <- !obs[i, ]
                  }
                }
            }
        }
        return(obs)

  }

  }

