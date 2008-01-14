`neighbourmap` <-
function(long, lat, var, W,listvar=NULL, listnomvar=NULL, carte=NULL,criteria=NULL,label="",symbol=0, labvar=c("",""),opt1=0,axis=FALSE, lablong="", lablat="") 
{
#initialisation

obs <- matrix(FALSE, nrow=length(long), ncol=length(long))
obs2 <- matrix(FALSE, nrow=length(long), ncol=length(long));
    nointer<-FALSE
    nocart<-FALSE
    buble<-FALSE
    legends<-list(FALSE,FALSE,"","")
    z<-NULL
    legmap<-NULL
    inout<-NULL
    graf<-"Neighbourplot1"

# Transformation data.frame en matrix

if(length(listvar)>0)
{
 if(dim(as.matrix(listvar))[2]==1)
 {
 listvar<-as.matrix(listvar)
 }
}


graphics.off();

get(getOption("device"))()
get(getOption("device"))()

fin <- tclVar(FALSE);
####################################################
# sélection d'un point sur la carte
####################################################

pointfunc<-function() 
{
   if (graf=="Neighbourplot2")
   {
    SGfunc();
   }
    quit <- FALSE;
    graf<<-"Neighbourplot1"
    while(!quit)
    {
        dev.set(2);
        loc<-locator(1);
        if (is.null(loc)) 
        {
            quit<-TRUE;
            next;
        }           
        obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point");

        # graphiques
        carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method="Neighbourplot1", W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
        graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", W=W,opt1=opt1, labvar=labvar, symbol=symbol);

  #  obs <<- matrix(FALSE, nrow=length(long), ncol=length(long));

    }
  }
     
####################################################
# sélection d'un polygone
####################################################

polyfunc<-function() 
{
   if (graf=="Neighbourplot2")
   {
    SGfunc();
   }
    graf<<-"Neighbourplot1"
    polyX <- NULL;
    polyY <- NULL;
    quit <- FALSE;

    while(!quit)
    {
        dev.set(2);
        loc<-locator(1);
        if(is.null(loc)) 
        {
            quit<-TRUE;
            next;
        }
        
        polyX <- c(polyX, loc[1]);
        polyY <- c(polyY, loc[2]);
        lines(polyX,polyY);
    }

    polyX <- c(polyX, polyX[1]);
    polyY <- c(polyY, polyY[1]);
if (length(polyX)>0)
{
    lines(polyX,polyY);

    obs <<- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly");
    
    # graphiques
    carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method="Neighbourplot1", W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
    graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", W=W, labvar=labvar, symbol=symbol,opt1=opt1);
 #   obs <<- matrix(FALSE, nrow=length(long), ncol=length(long));

}
  }

####################################################
# sélection d'un point sur le scatterplot
####################################################

voisfunc <- function()
{
   if (graf=="Neighbourplot1")
   {
    SGfunc();
   }
    graf<<-"Neighbourplot2"
    quit <- FALSE;

    while(!quit)
    {
        dev.set(3);
        loc<-locator(1);
        if(is.null(loc)) 
        {
            quit<-TRUE;
            next;
        }
        obs <<- selectstat(var1=var,obs=obs, Xpoly=loc[1], Ypoly=loc[2], method="Neighbourplot", W=W);

        # graphiques
        carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method="Neighbourplot2", W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
       graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", W=W, labvar=labvar, symbol=symbol,opt1=opt1);
    }
  }



####################################################
# sélection d'un polygone sur le scattermap
####################################################


 polyscatfunc <- function() 
 { obs2 <<- matrix(FALSE, nrow=length(long), ncol=length(long));
   if (graf=="Neighbourplot1")
   {
    SGfunc();
   }

    graf<<-"Neighbourplot2"
  quit <- FALSE
  polyX <- NULL
  polyY <- NULL
   while (!quit) 
    {
     dev.set(3)
     loc <- locator(1)
       if (is.null(loc)) 
         {
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

  
 obs2[which(W!=0,arr.ind=TRUE)] <<- inout(cbind(var[which(W!=0,arr.ind=TRUE)[,1]],var[which(W!=0,arr.ind=TRUE)[,2]]),cbind(polyX, polyY), bound = TRUE)

 obs3<-obs+obs2
 obs[which(obs3==1,arr.ind=TRUE)]<<-TRUE
 obs[which(obs3!=1,arr.ind=TRUE)]<<-FALSE


#ok<-cbind(var[which(W==1,arr.ind=TRUE)[,1]],var[which(W==1,arr.ind=TRUE)[,2]])
#pts<-inout(cbind(var[which(W==1,arr.ind=TRUE)[,1]],var[which(W==1,arr.ind=TRUE)[,2]]),cbind(polyX,polyY),bound=TRUE)

#for (k in 1:length(pts)) 
#{
#if(pts[k]==TRUE)
#  {for (i in 1:length(long))
#    { if(var[i]==ok[k,1])
#       {for (j in 1:length(long))  
#        {if(var[j]==ok[k,2])
#          {obs[i,j] <- TRUE} 
#        }
#       }
 #   }
#  }
#}


       carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method="Neighbourplot2", W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
       graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", W=W, labvar=labvar, symbol=symbol,opt1=opt1);
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
        carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method=graf, W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
           
       }
     else
       {
        tkmessageBox(message="Spatial Contours have not been given",icon="warning",type="ok")
        carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method=graf, W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
       
       }
  }

 else


 {
   nocart<<-FALSE;
        carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method=graf, W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
 
 }

}

####################################################
# rafraichissement des graphiques
####################################################

SGfunc<-function() 
{
    obs <<- matrix(FALSE, nrow=length(long), ncol=length(long));

    # graphiques
        carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method="Neighbourplot1", W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
     graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", W=W, labvar=labvar, symbol=symbol,opt1=opt1);
  }

####################################################
# Open a no interactive selection
####################################################

fnointer<-function() 
{
 if(!nointer)
  { if (length(criteria) != 0)
     {nointer<<-TRUE
      carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method=graf, W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
     }
   else
      {
       tkmessageBox(message="Criteria has not been given",icon="warning",type="ok")
      }

  }
 else 
 {
   nointer<<-FALSE;
   carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method=graf, W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)  
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
     varChoix <- choixvarfunc("Choice of variables","Choose a variable",listnomvar)
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
      carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method=graf, W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
  
   
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
rbValue="math"
tkdestroy(tt3)	
msg <- paste("Click on the map to indicate the location of the upper left corner of the legend box
")
tkmessageBox(message=msg)
dev.set(2);
loc <- locator(1)

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
      carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method=graf, W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)

           

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
rbValue="math"
tkdestroy(tt2)
if(rbValue=="math")
{
z<<-sqrt(bubble/max(bubble))*3
}
else
{
z<<-3*(bubble/max(bubble))^0.57
}	
      carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method=graf, W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
  
         
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
# quitter l'application
####################################################

quitfunc<-function() 
{
    tclvalue(fin)<<-TRUE
 #   graphics.off();
    tkdestroy(tt);
}


carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol, method="Neighbourplot1", W=W,axis=axis,legmap=legmap,legends=legends,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart)
graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", W=W, labvar=labvar, symbol=symbol,opt1=opt1);

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

point.but <- tkbutton(tt, text="  Point  ", command=pointfunc);
poly.but <- tkbutton(tt, text="  Polygon  ", command=polyfunc);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))

labelText71 <- tclVar("Work on the Graph")
label71 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText71))
tkconfigure(label71, textvariable=labelText71)
tkgrid(label71,columnspan=2)

vari.but <- tkbutton(tt, text="  Points  ", command=voisfunc);
vari2.but <- tkbutton(tt, text="  Polygon  ", command=polyscatfunc);
tkgrid(vari.but,vari2.but)
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

labelText9 <- tclVar("Bubbles")
label9 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText9))
tkconfigure(label9, textvariable=labelText9)
tkgrid(label9,columnspan=2)

bubble.but <- tkbutton(tt, text="  On/Off  ", command=fbubble);
tkgrid(bubble.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText5 <- tclVar("Exit")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))
tkwait.variable(fin)
}
####################################################


return(obs)
  }

