`histobarmap` <-
function(long,lat,var1,var2,criteria=NULL,carte=NULL,label = "",symbol = 0,color=1,nbcol = 10,labvar = c("", ""),labmod="", listvar=NULL, listnomvar=NULL,axis=FALSE,lablong="", lablat="")
{

#initialisation

    nointer<-FALSE
    nocart<-FALSE
    buble<-FALSE
    legends<-list(FALSE,FALSE,"","")
    z<-NULL
    legmap<-NULL

 if(labmod[1]=="")
 {labmod<-levels(as.factor(var1))}

    obs <- vector(mode = "logical", length = length(long))
    graphics.off()
get(getOption("device"))()
get(getOption("device"))()
get(getOption("device"))()
    fin <- tclVar(FALSE)
    graphChoice <- "";
    varChoice1 <- "";
    varChoice2 <- "";
    choix<-"";
    listgraph <- c("Histogram","Barplot","Scatterplot")

#transformation data.frame en matrice

if(length(listvar)>0)
{
 if(dim(as.matrix(listvar))[2]==1)
 {
 listvar<-as.matrix(listvar)
 }
}




####################################################
# sélection d'un point
####################################################

    pointfunc <- function() {
        quit <- FALSE
        while (!quit) {
            dev.set(2)
            loc <- locator(1)
            if (is.null(loc)) {
                quit <- TRUE
                next
            }
           obs <<- selectmap(var1 = long, var2 = lat, obs = obs,Xpoly = loc[1], Ypoly = loc[2], method = "point")
           carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
           graphique(var1 = var1, obs = obs, num = 3, graph = "Barplot",labvar = labvar[1], symbol = symbol, labmod = labmod,col=color)
           graphique(var1 = var2, obs = obs, num = 4, graph = "Histogram",nbcol = nbcol, labvar = labvar[2], symbol = symbol)

        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=5,graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }
        }
    }

####################################################
# sélection d'un polygone
####################################################

    polyfunc <- function() 
{
        polyX <- NULL
        polyY <- NULL
        quit <- FALSE
        while (!quit) {
            dev.set(2)
            loc <- locator(1)
            if (is.null(loc)) {
                quit <- TRUE
                next
            }
            polyX <- c(polyX, loc[1])
            polyY <- c(polyY, loc[2])
            lines(polyX, polyY)
        }
        polyX <- c(polyX, polyX[1])
        polyY <- c(polyY, polyY[1])
if (length(polyX)>0)
{
        lines(polyX, polyY)

        obs <<- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly");

        carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
        graphique(var1 = var1, obs = obs, num = 3, graph = "Barplot",labvar = labvar[1], symbol = symbol, labmod = labmod,col=color)
        graphique(var1 = var2, obs = obs, num = 4, graph = "Histogram",nbcol = nbcol, labvar = labvar[2], symbol = symbol)
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=5,graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }
}
}

####################################################
# sélection d'une barre sur le diagramme
####################################################

    bar1func <- function() {
        SGfunc()
        quit <- FALSE
        while (!quit) {
            dev.set(3)
            loc <- locator(1)
            if (is.null(loc)) {
                quit <- TRUE
                next
            }

           obs<<-selectstat(var1=var1,obs=obs,Xpoly=loc[1], Ypoly=loc[2],method="Barplot");    

           graphique(var1 = var1, obs = obs, num = 3, graph = "Barplot",labvar = labvar[1], symbol = symbol, labmod = labmod,col=color)
           graphique(var1 = var2, obs = obs, num = 4, graph = "Histogram",nbcol = nbcol, labvar = labvar[2], symbol = symbol)
           carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=5, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }
          
        }
    }

####################################################
# sélection d'une barre sur l'histogramme
####################################################

    bar2func <- function() {
        SGfunc()
        quit <- FALSE
        while (!quit) {
            dev.set(4)
            loc <- locator(1)
            if (is.null(loc)) {
                quit <- TRUE
                next
            }

           obs<<-selectstat(var1=var2,obs=obs,Xpoly=loc[1], Ypoly=loc[2],method="Histogram", nbcol=nbcol);   

           graphique(var1 = var1, obs = obs, num = 3, graph = "Barplot",labvar = labvar[1], symbol = symbol, labmod = labmod,col=color)
           graphique(var1 = var2, obs = obs, num = 4, graph = "Histogram",nbcol = nbcol, labvar = labvar[2], symbol = symbol)
           carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=5,graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }
         }
    }



####################################################
#  d'un autre graphique
####################################################

graphfunc <- function()
{
    if ((length(listvar) != 0) && (length(listnomvar) != 0))
    {
        dev.off(5);
        choix <<- selectgraph(listnomvar,listgraph);
        varChoice1 <<- choix$varChoice1;
        varChoice2 <<- choix$varChoice2;
        graphChoice <<- choix$graphChoice;
            
        if ((graphChoice != "") && (varChoice1 != ""))
        {
            x11(width=5, height=5);
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=5,graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
            }
    }
    else
    {
        tkmessageBox(message="Listvar and listnomvar must have been given in input",icon="warning",type="ok");
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
        carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
        }
     else
       {
        tkmessageBox(message="Spatial Contours have not been given",icon="warning",type="ok")
        carte(long=long, lat=lat,criteria=criteria,nointer=nointer,buble=buble,cbuble=z,obs=obs,  label=label, symbol=symbol,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
       }
  }

 else


 {
   nocart<<-FALSE;
   graphique(var1 = var1, obs = obs, num = 3, graph = "Barplot",labvar = labvar[1], symbol = symbol, labmod = labmod,col=color)
   graphique(var1 = var2, obs = obs, num = 4, graph = "Histogram",nbcol = nbcol, labvar = labvar[2], symbol = symbol)
   carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=5,graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }
        
 }

}


####################################################
# rafraichissement des graphiques
####################################################


    SGfunc <- function() {
        obs <<- vector(mode = "logical", length = length(long))
        graphique(var1 = var1, obs = obs, num = 3, graph = "Barplot",labvar = labvar[1], symbol = symbol, labmod = labmod,col=color)
        graphique(var1 = var2, obs = obs, num = 4, graph = "Histogram",nbcol = nbcol, labvar = labvar[2], symbol = symbol)
        carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=5, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }
         }


####################################################
# quitter l'application
####################################################


    quitfunc <- function() {
        tclvalue(fin) <- TRUE
 #       graphics.off()
        tkdestroy(tt)
    }

####################################################
# Open a none interactive selection
####################################################

fnointer<-function() 
{
 if(!nointer)
  { if (length(criteria) != 0)
     {nointer<<-TRUE
           carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
        
     }
   else
      {
       tkmessageBox(message="Criteria has not been given",icon="warning",type="ok")
      }

  }

 else 

 {
   nointer<<-FALSE;
           carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
   
 }

}


####################################################
# Choisir une variable
####################################################

    choixvarfunc <- function(title, question, liste) {
        tt2 <- tktoplevel()
        scr <- tkscrollbar(tt2, repeatinterval = 5, command = function(...) tkyview(lstbox,...))
        lstbox <- tklistbox(tt2, height = 4, selectmode = "single",yscrollcommand = function(...) tkset(scr, ...), background = "white")
        tkgrid(tklabel(tt2, text = question))
        tkfocus(tt2)
        tkwm.title(tt2, title)

        varChoice <- ""
        tkgrid(lstbox, scr)
        tkgrid.configure(scr, rowspan = 4, sticky = "nsw")
        var <- liste

        for (i in 1:length(var)) 
        {
         tkinsert(lstbox, "end", var[i])
        }

        OnOK <- function() 
        {
         varChoice <<- var[as.numeric(tkcurselection(lstbox)) + 1]
         tkdestroy(tt2)
        }

        OK.but <- tkbutton(tt2, text = "   OK   ", command = OnOK)
        tkgrid(OK.but)
        tkbind(tt2, "<Destroy>", function() {tkdestroy(tt2)})
        tkfocus(tt2)
        tkwait.window(tt2)
        return(varChoice)
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
 carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
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
carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis)


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
carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
 
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



carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")
graphique(var1 = var1, obs = obs, num = 3, graph = "Barplot",labvar = labvar[1], symbol = symbol, labmod = labmod,col=color)
graphique(var1 = var2, obs = obs, num = 4, graph = "Histogram",nbcol = nbcol, labvar = labvar[2], symbol = symbol)
 

####################################################
# création de la boite de dialogue to create legens
####################################################

if(interactive())
{
OnOK <- function()
{ 
tkdestroy(tt1)	
msg <- paste("Click on the map to indicate the location of the upper left corner of the legend box")
	tkmessageBox(message=msg)

dev.set(2);
loc <- locator(1)
legends<<-list(legends[[1]],TRUE,legends[[4]],loc)

carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")

}

OnOK2 <- function()
{ 
legends<<-list(legends[[2]],FALSE,legends[[4]],"")
tkdestroy(tt1)	

carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,col=color,method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong="", lablat="")

}


if(color==1)
{
tt1<-tktoplevel()

labelText12 <- tclVar("Do you want a legend for factors")
label12 <- tklabel(tt1,justify = "center", wraplength = "3i", text=tclvalue(labelText12))
tkconfigure(label12, textvariable=labelText12)
tkgrid(label12,columnspan=2)

point.but <- tkbutton(tt1, text="  Yes  ", command=OnOK);
poly.but <- tkbutton(tt1, text=" No ", command=OnOK2);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt1,text="    "))

}

tkfocus(tt1)
}

####################################################
# création de la boite de dialogue
####################################################
if(interactive())
{
tt <- tktoplevel()

labelText1 <- tclVar("Work on the map")
label1 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)

point.but <- tkbutton(tt, text="   Points   ", command=pointfunc);
poly.but <- tkbutton(tt, text="   Polygon   ", command=polyfunc);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))


labelText2 <- tclVar("Work on a graph")
label2 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText2))
tkconfigure(label2, textvariable=labelText2)
tkgrid(label2,columnspan=2)


barre1.but <- tkbutton(tt, text=" Bar plot ", command=bar1func);
barre2.but <- tkbutton(tt, text=" Histogram ", command=bar2func);
tkgrid(barre1.but,barre2.but)
tkgrid(tklabel(tt,text="    "))


label1 <- tclVar("To stop selection, let the cursur on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
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


labelText9 <- tclVar("  Bubbles  ")
label9 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText9))
tkconfigure(label9, textvariable=labelText9)
tkgrid(label9,columnspan=2)

bubble.but <- tkbutton(tt, text="  On/Off  ", command=fbubble);
tkgrid(bubble.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText4 <- tclVar("Additional graph")
label4 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText4))
tkconfigure(label4, textvariable=labelText4)
tkgrid(label4,columnspan=2)

autre.but <- tkbutton(tt, text="     OK     " , command=graphfunc);
tkgrid(autre.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText5 <- tclVar("  Exit  ")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

tkwait.variable(fin);
}

####################################################
# Fin
####################################################
   
return(obs);
}

