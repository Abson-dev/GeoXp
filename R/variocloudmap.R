`variocloudmap` <-
function (long, lat, var,bin=NULL,quantiles=NULL,listvar=NULL, listnomvar=NULL,criteria=NULL,carte = NULL, label = "",symbol = 0, labvar = "",axis=FALSE,lablong = "", lablat = "") 
{
# initialisation

    nointer<-FALSE
    nocart<-FALSE
    buble<-FALSE
    legends<-list(FALSE,FALSE,"","")
    z<-NULL 
    legmap<-NULL
    inout<-NULL

    opt1<-1
    opt2<-1
#    dat<-cbind(long,lat,var)
#   dat<-data.frame(cbind(long,lat,var))
    angle<-0
    obs <- matrix(FALSE, nrow = length(long), ncol = length(long))
    graphics.off()

get(getOption("device"))()
get(getOption("device"))()
    fin <- tclVar(FALSE)

# Transformation data.frame en matrix
    
    if(dim(as.matrix(listvar))[2]==1)
    {
     listvar<-as.matrix(listvar)
    }

####################################################
# calcul des matrices diff et dist
####################################################

  long1 <- matrix(rep(t(long), length(long)), ncol = dim(t(long))[2],byrow = FALSE)
  long2 <- matrix(rep(t(long), length(long)), ncol = dim(t(long))[2],byrow = TRUE)
    
  lat1 <- matrix(rep(t(lat), length(lat)), ncol = dim(t(lat))[2],byrow = FALSE)
  lat2 <- matrix(rep(t(lat), length(lat)), ncol = dim(t(lat))[2],byrow = TRUE)
  
  v1 <- matrix(rep(t(var), length(var)), ncol = dim(t(var))[2],byrow = FALSE)
  v2 <- matrix(rep(t(var), length(var)), ncol = dim(t(var))[2],byrow = TRUE)

 theta <- matrix(0, nrow = length(long), ncol = length(long))
 numer <- lat2 - lat1
 denom <- long2 - long1
   

   for (i in 1:length(long)) 
       {
        for (j in 1:length(long)) 
           {
             if (denom[i, j] == 0) 
               {
                theta[i, j] <- pi/2
               }
             else 
               {
                 theta[i, j] <- atan(numer[i, j]/denom[i, j])
               }
           }
       }

  theta[which(theta < 0)] <- theta[which(theta < 0)] + pi

  dist <- sqrt((long1 - long2)^2 + (lat1 - lat2)^2)
  dif <-  (v1 - v2)^2/2
  dif2 <-  (abs(v1 - v2))^(1/2)


####################################################
#choix des bornes des réglettes (inspiré de la documentation de Matlab)
####################################################

        v4 <- sort(dist)
        v4 <- as.vector(v4)
        z <- seq(1, max(v4), by = (max(v4)/1000))
        z <- round(z)
        z1 <- z[2:length(z)] - z[1:(length(z) - 1)]
        h <- mean(z1)
        p <- 1/(1 + (h^3/6))
        p1 <- 1/(1 + (h^3/60))
        p2 <- 1/(1 + (h^3/0.6))
        alpha <- (1 - p)/p
        borne1 <- (1 - p1)/p1
        borne2 <- (1 - p2)/p2



####################################################
# sélection d'un point sur le variocloud
####################################################

    pointfunc <- function() 
     {
        quit <- FALSE
        while (!quit) 
        {
            dev.set(3)
            loc <- locator(1)
            if (is.null(loc)) {
                quit <- TRUE
                next
            }
     obs <<- selectstat(var1 = dist, var2 = dif, obs = obs,Xpoly = loc[1], Ypoly = loc[2], method = "Variopoint",long = long, lat = lat)
     graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs,opt1=opt1,opt2=opt2, num = 3, graph = "Variocloud", labvar = labvar, symbol = symbol, quantiles = quantiles, alpha1 = alpha,bin=bin)
     carte(long = long, lat = lat, obs = obs, lablong = lablong,lablat = lablat, label = label,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart, symbol = symbol,method = "Variocloud",axis=axis,legmap=legmap,legends=legends)
        }
    }

####################################################
# sélection d'un polygone sur l'angleplot
####################################################

    polyfunc <- function() {
        quit <- FALSE
        polyX <- NULL
        polyY <- NULL
        while (!quit) {
            dev.set(3)
            loc <- locator(1)
            if (is.null(loc)) {
                quit <- TRUE
                next
            }
            polyX <- c(polyX, loc[1])
            polyY <- c(polyY, loc[2])
           if (length(polyX)>0)
           {
            lines(polyX, polyY)
           }
        }
        polyX <- c(polyX, polyX[1])
        polyY <- c(polyY, polyY[1])
if (length(polyX)>0)
{
        lines(polyX, polyY)
        for (i in 1:length(long)) {
            obs[, i] <<- inout(cbind(dist[, i], dif[, i]), cbind(polyX, 
                polyY), bound = TRUE)
        }
        graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3, opt1=opt1,opt2=opt2,
            graph = "Variocloud", labvar = labvar, symbol = symbol, 
            quantiles = quantiles, alpha1 = alpha,bin=bin)
        carte(long = long, lat = lat, obs = obs, lablong = lablong, legmap=legmap,
            lablat = lablat, label = label, symbol = symbol,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart, 
            method = "Variocloud",axis=axis,legends=legends)
}
    }



####################################################
# contour des unités spatiales
####################################################
cartfunc <- function()
{
  if(!nocart)
   { if (length(carte) != 0)
       {nocart<<-TRUE
        carte(long=long, lat=lat,criteria=criteria,buble=buble,cbuble=z,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart, 
              method = "Variocloud",axis=axis,legends=legends,legmap=legmap)
       }
     else
       {
        tkmessageBox(message="Spatial Contours have not been given",icon="warning",type="ok")
        carte(long=long, lat=lat,criteria=criteria,nointer=nointer,buble=buble,cbuble=z,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, 
            method = "Variocloud",legmap=legmap,axis=axis,legends=legends)
       }
  }

 else


 {
   nocart<<-FALSE;
   carte(long=long, lat=lat,buble=buble,criteria=criteria,nointer=nointer,legmap=legmap,cbuble=z,obs=obs,method="Variocloud", lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart, axis=axis,legends=legends)

 }

}


####################################################
# Pour le alpha 
####################################################
    refresh.code <- function(...) {
        alpha <<- slider1(no = 1)
        graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3, opt1=opt1,opt2=opt2,
            graph = "Variocloud", labvar = labvar, symbol = symbol, 
            quantiles = quantiles, alpha1 = alpha,bin=bin)
    }



####################################################
# rafraichissement des graphiques
####################################################

    SGfunc <- function() {
        obs <<- matrix(FALSE, nrow = length(long), ncol = length(long))
      carte(long=long, lat=lat,obs=obs,buble=buble,criteria=criteria,legmap=legmap,nointer=nointer,cbuble=z,carte=carte,nocart=nocart,method = "Variocloud", lablong=lablong, lablat=lablat, label=label, symbol=symbol,axis=axis,legends=legends)
       graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3,opt1=opt1, opt2=opt2,
            graph = "Variocloud", labvar = labvar, symbol = symbol, 
            quantiles = quantiles, alpha1 = alpha,bin=bin)
    }


####################################################
# quitter l'application
####################################################

    quitfunc <- function() {
        tclvalue(fin) <<- TRUE
   #     graphics.off()
        tkdestroy(tt)
    }


####################################################
# Open a no interactive selection
####################################################

fnointer<-function() 
{
 if(!nointer)
  { if (length(criteria) != 0)
     {nointer<<-TRUE
      carte(long=long, lat=lat,obs=obs,buble=buble,criteria=criteria,legmap=legmap,nointer=nointer,cbuble=z,carte=carte,nocart=nocart,method = "Variocloud", lablong=lablong, lablat=lablat, label=label, symbol=symbol,axis=axis,legends=legends)

     }
   else
      {
       tkmessageBox(message="Criteria has not been given",icon="warning",type="ok")
      }

  }

 else 

 {
   nointer<<-FALSE;
   carte(long=long, lat=lat,buble=buble,criteria=criteria,legmap=legmap,nointer=nointer,cbuble=z,obs=obs, lablong=lablong, lablat=lablat, label=label,symbol=symbol,carte=carte,nocart=nocart, 
            method = "Variocloud",axis=axis,legends=legends)
  
 }

}


####################################################
# Bubble
####################################################

fbubble<-function(){

if(!buble)
{
   if ((length(listvar) != 0) && (length(listnomvar) != 0))
    {


     varChoix <- choixvarfunc("Choice of variables","Choose the variable",listnomvar)
      bubble <- listvar[,which(listnomvar == varChoix)]
      if ((length(bubble) != 0)&&(min(bubble)>=0))
       { 
         buble<<-TRUE   
       } 
      else
       {
        tkmessageBox(message="Bubbles have not been given or must be positiv",icon="warning",type="ok")
       }

    }

else
    {
        tkmessageBox(message="To use Bubbles, the lists wich contain the variables and their names must have been given",icon="warning",type="ok");
    }

}

else
{buble<<-FALSE
 legends<<-list(FALSE,legends[[2]],"",legends[[4]])
   carte(long=long, lat=lat,buble=buble,criteria=criteria,legmap=legmap,nointer=nointer,cbuble=z,obs=obs, lablong=lablong, lablat=lablat, label=label,symbol=symbol,carte=carte,nocart=nocart, 
            method = "Variocloud",axis=axis,legends=legends)
}




if(buble)
{

tt2 <- tktoplevel()

OnOK <- function()
{ 
tkdestroy(tt2)	

tt3 <- tktoplevel()


bubl<-function()
{
tkdestroy(tt3)	
msg <- paste("Click on the map to indicate the location of the upper left corner of the legend box
")
tkmessageBox(message=msg)
dev.set(2);
loc <- locator(1)
rbValue="math"
if(rbValue=="math")
{
z<<-sqrt(bubble/max(bubble))*3
legmap<<-c(sqrt(as.numeric(tclvalue(ma))/max(bubble))*3,sqrt(as.numeric(tclvalue(mea))/max(bubble))*3,sqrt(as.numeric(tclvalue(mi))/max(bubble))*3,as.numeric(tclvalue(ma)),as.numeric(tclvalue(mea)),as.numeric(tclvalue(mi)),varChoix)
}
else
{
z<<-3*(bubble/max(bubble))^0.57
legmap<<-c(3*(as.numeric(tclvalue(ma))/max(bubble))^0.57,3*(as.numeric(tclvalue(mea))/max(bubble))^0.57,3*(as.numeric(tclvalue(mi))/max(bubble))^0.57,as.numeric(tclvalue(ma)),as.numeric(tclvalue(mea)),as.numeric(tclvalue(mi)),varChoix)
}


legends<<-list(TRUE,legends[[2]],loc,legends[[4]])
   carte(long=long, lat=lat,buble=buble,criteria=criteria,legmap=legmap,nointer=nointer,cbuble=z,obs=obs, lablong=lablong, lablat=lablat, label=label,symbol=symbol,carte=carte,nocart=nocart, 
            method = "Variocloud",axis=axis,legends=legends)


}


mi<-tclVar(round(min(bubble),2))
mea<-tclVar(round(mean(bubble),2))
ma<-tclVar(round(max(bubble),2))
entry.Name <-tkentry(tt3,width="5",textvariable=mi)
entry.Name2 <-tkentry(tt3,width="5",textvariable=mea)
entry.Name3 <-tkentry(tt3,width="5",textvariable=ma)
tkgrid(tklabel(tt3,text="Break Points:"))
tkgrid(tklabel(tt3,text="Small Bubble"),entry.Name)
tkgrid(tklabel(tt3,text="Middle Bubble"),entry.Name2)
tkgrid(tklabel(tt3,text="Large Bubble"),entry.Name3)

autre.but <- tkbutton(tt3, text="     OK     " , command=bubl);
tkgrid(autre.but,columnspan=2)
tkgrid(tklabel(tt3,text="    "))
tkfocus(tt3)

}

OnOK2 <- function()
{ 
tkdestroy(tt2)
rbValue="math"
if(rbValue=="math")
{
z<<-sqrt(bubble/max(bubble))*3
}
else
{
z<<-3*(bubble/max(bubble))^0.57
}	
   carte(long=long, lat=lat,buble=buble,criteria=criteria,legmap=legmap,nointer=nointer,cbuble=z,obs=obs, lablong=lablong, lablat=lablat, label=label,symbol=symbol,carte=carte,nocart=nocart, 
            method = "Variocloud",axis=axis,legends=legends)

}


labelText12 <- tclVar("Do you want a legend for bubbles")
label12 <- tklabel(tt2,justify = "center", wraplength = "3i", text=tclvalue(labelText12))
tkconfigure(label12, textvariable=labelText12)
tkgrid(label12,columnspan=2)

point.but <- tkbutton(tt2, text="  Yes  ", command=OnOK);
poly.but <- tkbutton(tt2, text=" No ", command=OnOK2);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt2,text="    "))
tkfocus(tt2)
}



}

####################################################
# Dessin du variogramme 
####################################################


vari<-function()
{
if(opt1==1)
{opt1<<-2;
        graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3,opt1=opt1,opt2=opt2,
          graph = "Variocloud", labvar = labvar, symbol = symbol, 
          quantiles = quantiles, alpha1 = alpha,bin=bin)

}

else
{opt1<<-1;
        graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3,opt1=opt1,opt2=opt2,
          graph = "Variocloud", labvar = labvar, symbol = symbol, 
          quantiles = quantiles, alpha1 = alpha,bin=bin)
}

}

vari2<-function()
{
if(opt2==1)
{opt2<<-2;
        graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3,opt1=opt1,opt2=opt2,
          graph = "Variocloud", labvar = labvar, symbol = symbol, 
          quantiles = quantiles, alpha1 = alpha,bin=bin)
}

else
{opt2<<-1;
        graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3,opt1=opt1,opt2=opt2,
          graph = "Variocloud", labvar = labvar, symbol = symbol, 
          quantiles = quantiles, alpha1 = alpha,bin=bin)
}

}
####################################################
# Choisir une variable pour les bubbles
####################################################

    choixvarfunc <- function(title, question, liste) {
        tt2 <- tktoplevel()
        scr <- tkscrollbar(tt2, repeatinterval = 5, command = function(...) tkyview(lstbox, 
            ...))
        lstbox <- tklistbox(tt2, height = 4, selectmode = "single", 
            yscrollcommand = function(...) tkset(scr, ...), background = "white")
        tkgrid(tklabel(tt2, text = question))
        tkfocus(tt2)
        tkwm.title(tt2, title)
        varChoice <- ""
        tkgrid(lstbox, scr)
        tkgrid.configure(scr, rowspan = 4, sticky = "nsw")
        var <- liste
        for (i in 1:length(var)) {
            tkinsert(lstbox, "end", var[i])
        }
        OnOK <- function() {
            varChoice <<- var[as.numeric(tkcurselection(lstbox)) + 
                1]
            tkdestroy(tt2)
        }
        OK.but <- tkbutton(tt2, text = "   OK   ", command = OnOK)
        tkgrid(OK.but)
        tkbind(tt2, "<Destroy>", function() {
            tkdestroy(tt2)
        })
        tkfocus(tt2)
        tkwait.window(tt2)
        return(varChoice)
    }

####################################################
# Choisir angle
####################################################




    choixangle <- function() {
SGfunc()
tt1<-tktoplevel()
Name <- tclVar("0.5")
entry.Name <-tkentry(tt1,width="3",textvariable=Name)
tkgrid(tklabel(tt1,text="Please enter a decimal x between 0 and 1 (angle=x.Pi)"),entry.Name)




OnOK <- function()
{ 
	angle <<- tclvalue(Name)
	tkdestroy(tt1)
       
       if (is.na(as.numeric(angle))||(as.numeric(angle)>1)||(as.numeric(angle)<0))
    {
        tkmessageBox(message="Sorry, but you have to choose a decimal number between 0 and 1 (exemple : 0.5)",icon="warning",type="ok");
    }
else
        {msg <- paste("You choose",angle,"pi")
	tkmessageBox(message=msg)
    
  dist <<- sqrt((long1 - long2)^2 + (lat1 - lat2)^2)
  dif <<-  (v1 - v2)^2
  dif2 <<-  (abs(v1 - v2))^(1/2)

if(as.numeric(angle)<1/10)
{ dist[which((theta>as.numeric(angle)*pi+pi/10)&(theta<9*pi/10+as.numeric(angle)*pi))] <<- -10
 dif[which((theta>as.numeric(angle)*pi+pi/10)&(theta<9*pi/10+as.numeric(angle)*pi))] <<- -10
 dif2[which((theta>as.numeric(angle)*pi+pi/10)&(theta<9*pi/10+as.numeric(angle)*pi))] <<- -10}

else if (as.numeric(angle)>9/10)
{ dist[which((theta>-2*pi+(as.numeric(angle)*pi+11*pi/10))&(theta<as.numeric(angle)*pi-pi/10))] <<- -10
 dif[which((theta>-2*pi+(as.numeric(angle)*pi+11*pi/10))&(theta<as.numeric(angle)*pi-pi/10))] <<- -10
 dif2[which((theta>-2*pi+(as.numeric(angle)*pi+11*pi/10))&(theta<as.numeric(angle)*pi-pi/10))] <<- -10}

else
{
 dist[which((theta>as.numeric(angle)*pi+pi/10))] <<- -10
 dist[which((theta<as.numeric(angle)*pi-pi/10))] <<- -10
 dif[which((theta>as.numeric(angle)*pi+pi/10))] <<- -10
 dif[which((theta<as.numeric(angle)*pi-pi/10))] <<- -10
 dif2[which((theta>as.numeric(angle)*pi+pi/10))] <<- -10
 dif2[which((theta<as.numeric(angle)*pi-pi/10))] <<- -10
}
        graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3, opt1=opt1,opt2=opt2,
            graph = "Variocloud", labvar = labvar, symbol = symbol, 
            quantiles = quantiles, alpha1 = alpha,bin=bin)
       }
}



OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
#tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(tt1)


    }

####################################################
# Return to Isotropy Variogram Cloud
####################################################


OnOK2 <- function()
{
SGfunc()
     dist <<- sqrt((long1 - long2)^2 + (lat1 - lat2)^2)
     dif <<-  (v1 - v2)^2/2
     dif2 <<-  (abs(v1 - v2))^(1/2)

        graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3, opt1=opt1,opt2=opt2,
            graph = "Variocloud", labvar = labvar, symbol = symbol, 
            quantiles = quantiles, alpha1 = alpha,bin=bin)
}



carte(long = long, lat = lat, obs = obs, lablong = lablong,lablat = lablat, label = label, symbol = symbol, 
method = "Variocloud",axis=axis)

graphique(var1 = dist, var2 = dif,var3=dif2, obs = obs, num = 3, opt1=opt1,opt2=opt2,
graph = "Variocloud", labvar = labvar, symbol = symbol, 
quantiles = quantiles, alpha1 = alpha)
            
####################################################
# création de la boite de dialogue
####################################################

if(interactive())
{
tt <- tktoplevel()

labelText1 <- tclVar("Work on the graph")
label1 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)

point.but <- tkbutton(tt, text="  Point  ", command=pointfunc);
poly.but <- tkbutton(tt, text=" Polygon ", command=polyfunc);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))

label1 <- tclVar("To stop selection, let the cursur on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
tkgrid(tklabel(tt,text="    "))


  if(length(quantiles)!=0)
{ slider1(tt, refresh.code, c("Quant. reg. smooth spline para."), 
            c(borne1), c(borne2), c((borne2 - borne1)/100), c(alpha))
}
   

labelText71 <- tclVar("Empirical Semi-Variogram")
label71 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText71))
tkconfigure(label71, textvariable=labelText71)
tkgrid(label71,columnspan=2)

vari.but <- tkbutton(tt, text="  Classic - On/Off  ", command=vari);
vari2.but <- tkbutton(tt, text=" Robust - On/Off ", command=vari2);
tkgrid(vari.but, vari2.but)
tkgrid(tklabel(tt,text="    "))


   
labelText73 <- tclVar("Directional Semi-Variogram Cloud")
label73 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText73))
tkconfigure(label73, textvariable=labelText73)
tkgrid(label73,columnspan=2)


vari3.but <- tkbutton(tt, text="     On     ", command=choixangle);
finish.but<-tkbutton(tt,text="     Off     ",command=OnOK2)
tkgrid(vari3.but,finish.but)
tkgrid(tklabel(tt,text="    "))




labelText7 <- tclVar("Non interactive selection")
label7 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText7))
tkconfigure(label7, textvariable=labelText7)
tkgrid(label7,columnspan=2)

noint1.but <- tkbutton(tt, text="  On/Off  ", command=fnointer);
tkgrid(noint1.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

labelText6 <- tclVar("Draw spatial contours")
label6 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText6))
tkconfigure(label6, textvariable=labelText6)
tkgrid(label6,columnspan=2)

nocou1.but <- tkbutton(tt, text="  On/Off  ", command=cartfunc);
tkgrid(nocou1.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText3 <- tclVar("Restore graph")
label3 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText3))
tkconfigure(label3, textvariable=labelText3)
tkgrid(label3,columnspan=2)

nettoy.but <- tkbutton(tt, text="     OK     " , command=SGfunc);
tkgrid(nettoy.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

labelText9 <- tclVar("Bubbles")
label9 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText9))
tkconfigure(label9, textvariable=labelText9)
tkgrid(label9,columnspan=2)

bubble.but <- tkbutton(tt, text="  On/Off  ", command=fbubble);
tkgrid(bubble.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText5 <- tclVar("  Exit  ")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

tkwait.variable(fin)
}

return(obs)

}

