`boxplotmap` <-
function(long,lat,var,listvar=NULL, listnomvar=NULL,carte=NULL,criteria=NULL,label="",symbol=0,color=1,labvar="",axis=FALSE, lablong="", lablat="")
{
# initialisation

nointer<-FALSE
nocart<-FALSE
buble<-FALSE
legends<-list(FALSE,FALSE,"","")
z<-NULL
legmap<-NULL

# Transformation data.frame en matrix

if(length(listvar)>0)
{
 if(dim(as.matrix(listvar))[2]==1)
 {
 listvar<-as.matrix(listvar)
 }
}

if(labvar=="")
 {labvar<-names(data.frame(var))}

var=as.matrix(var)
lat=as.matrix(lat)
long=as.matrix(long)


obs<-vector(mode = "logical", length = length(long))
graphics.off();

get(getOption("device"))()
get(getOption("device"))()

fin <- tclVar(FALSE);

graphChoice <- "";
varChoice1 <- "";
varChoice2 <- "";
choix<-""
listgraph <- c("Histogram","Barplot","Scatterplot")

####################################################
# sélection d'une partie du boxplot
####################################################

boxfunc<-function()
{
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
        obs<<-selectstat(var1=var,obs=obs,Xpoly=loc[1], Ypoly=loc[2],method="Boxplot");

        # graphiques
        graphique(var1=var, obs=obs, num=3, graph="Boxplot", labvar=labvar, symbol=symbol);

        # Remarque : s'il y a tous ces If, c'est pour prévoir de rajoutter un barplot avec options de couleurs
        # sur la carte 

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2),col=color);
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
            carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         }  
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
      # graphiques

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         }  


    }
     else
       {
        tkmessageBox(message="Spatial contours have not been given",icon="warning",type="ok")
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         }        }
  }

 else


 {
   nocart<<-FALSE;
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         }  
 }

}



####################################################
# choix d'un autre graphique
####################################################

graphfunc <- function()
{ 
    if ((length(listvar) != 0) && (length(listnomvar) != 0))
    {
        dev.off(4);
        choix <<- selectgraph(listnomvar,listgraph);
        varChoice1 <<- choix$varChoice1;
        varChoice2 <<- choix$varChoice2;
        graphChoice <<- choix$graphChoice;
            
        if ((graphChoice != "") && (varChoice1 != ""))
        {

      if (((graphChoice == "Histogram")&&(!is.numeric(listvar[,which(listnomvar == varChoice1)])))||((graphChoice == "Scatterplot")&&((!is.numeric(listvar[,which(listnomvar == varChoice1)]))||(!is.numeric(listvar[,which(listnomvar == varChoice2)]))))) 
        {
        tkmessageBox(message="Variables choosed are not in a good format",icon="warning",type="ok");
        }
       else
           { 
            x11(width=5, height=5);
            if(graphChoice=="Barplot")
             {

               tt1<-tktoplevel()

OnOK <- function()
{ 

 tkdestroy(tt1)	
 msg <- paste("Click on the map to indicate the location of the upper left corner of the legend box")
 tkmessageBox(message=msg)

 dev.set(2);
 loc <- locator(1)
 legends<<-list(legends[[1]],TRUE,legends[[3]],loc)


 labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
 graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2),labmod=labmod,col=color);
 carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong=lablong,lablat=lablat)
   
}


OnOK2 <- function()
{ 

 legends<<-list(legends[[1]],FALSE,legends[[3]],"")
 tkdestroy(tt1)	

 labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
 graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2),labmod=labmod,col=color);
 carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong=lablong,lablat=lablat)
  
}



labelText12 <- tclVar("Do you want a legend for factors")
label12 <- tklabel(tt1,justify = "center", wraplength = "3i", text=tclvalue(labelText12))
tkconfigure(label12, textvariable=labelText12)
tkgrid(label12,columnspan=2)

point.but <- tkbutton(tt1, text="  Yes  ", command=OnOK);
poly.but <- tkbutton(tt1, text=" No ", command=OnOK2);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))


tkfocus(tt1)             }
           else
            {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
            carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } }
       }
    }
    else
    {
        tkmessageBox(message="Variables or variable names have not been given",icon="warning",type="ok");
    }
  }


####################################################
# rafraichissement des graphiques
####################################################

SGfunc<-function() 
{
    obs<<-vector(mode = "logical", length = length(long));

 # graphiques

        graphique(var1=var, obs=obs, num=3, graph="Boxplot", labvar=labvar, symbol=symbol);

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2),labmod=labmod,col=color);
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
            carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         }  

}

####################################################
# quitter l'application
####################################################

quitfunc<-function() 
{
    tclvalue(fin)<<-TRUE
#    graphics.off();
    tkdestroy(tt);
  }




####################################################
# Open a no interactive selection
####################################################

fnointer<-function() 
{
 if(!nointer)
  { if (length(criteria) != 0)
     {nointer<<-TRUE
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         }    }
   else
      {
       tkmessageBox(message="Criteria has not been given",icon="warning",type="ok")
      }

  }

 else 

 {
   nointer<<-FALSE;

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         }  
 }

}

####################################################
# Choisir une variable
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
     if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         }  
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
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong=lablong, lablat=lablat)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
  
         }   

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
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         } 
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



carte(long=long, lat=lat,criteria=criteria,nointer=nointer,buble=buble,cbuble=z,obs=obs, label=label, symbol=symbol,carte=carte,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
graphique(var1=var, obs=obs, num=3, graph="Boxplot", labvar=labvar, symbol=symbol);


####################################################
# création de la boite de dialogue
####################################################
if(interactive())
{
tt <- tktoplevel();

labelText2 <- tclVar("Work on the boxplot")
label2 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText2))
tkconfigure(label2, textvariable=labelText2)
tkgrid(label2,columnspan=2)

barre.but <- tkbutton(tt, text="Boxplot", command=boxfunc);
tkgrid(barre.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))




label1 <- tclVar("To stop selection, leave the cursur on the active graph, click on the right button of the mouse and stop")
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



labelText9 <- tclVar("  Bubbles  ")
label9 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText9))
tkconfigure(label9, textvariable=labelText9)
tkgrid(label9,columnspan=2)

bubble.but <- tkbutton(tt, text="  On/Off  ", command=fbubble);
tkgrid(bubble.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

labelText3 <- tclVar("Restore graph")
label3 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText3))
tkconfigure(label3, textvariable=labelText3)
tkgrid(label3,columnspan=2)

nettoy.but <- tkbutton(tt, text="     OK     " , command=SGfunc);
tkgrid(nettoy.but,columnspan=2)
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
tkwait.variable(fin)

}
####################################################

return(obs)
  }

