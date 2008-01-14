`driftmap` <-
function(long,lat,var,interpol=1,nuage=0,carte=NULL,lablong="",lablat="",labvar="")
{

# initialisation
    theta=0
    nbrow=10
    nbcol=10
    graphics.off()
get(getOption("device"))()
    fin <- tclVar(FALSE)

if(interpol==1)
{symbol = 1}
else
{symbol = 0}

choixangle <- function() 
{
tt1<-tktoplevel()
Name <- tclVar("0")
entry.Name <-tkentry(tt1,width="3",textvariable=Name)
tkgrid(tklabel(tt1,text="Please enter a angle in degree"),entry.Name)


OnOK <- function()
{ 
	theta <- tclvalue(Name)
	tkdestroy(tt1)
       
   if (is.na(as.numeric(theta))||(as.numeric(theta)<0))
     {
      tkmessageBox(message="Sorry, but you have to choose a positive number",icon="warning",type="ok");
     }
   else
     {
      msg <- paste("You choose",theta,"degrees")
      tkmessageBox(message=msg)
      theta<<-as.numeric(theta)
      graphique()
     }
}



OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(tt1)

}


####################################################
# Choisir numbre of grids
####################################################

choixgrid <- function() 
{
tt1<-tktoplevel()
Name1 <- tclVar("10")
Name2 <- tclVar("10")
entry.Name <-tkentry(tt1,width="3",textvariable=Name1)
tkgrid(tklabel(tt1,text="Enter number of rows"),entry.Name)
entry.Name2 <-tkentry(tt1,width="3",textvariable=Name2)
tkgrid(tklabel(tt1,text="Enter number of columns"),entry.Name2)



OnOK <- function()
{ 
	nbrow <<- tclvalue(Name1)
	nbcol <<- tclvalue(Name2)
	tkdestroy(tt1)
       
   if (is.na(as.numeric(nbrow ))||(as.numeric(nbrow)<2)||(as.numeric(nbcol)<2))
     {
      tkmessageBox(message="Sorry, but you have to choose a number upper than 2",icon="warning",type="ok");
     }
   else
     {

      nbrow<<-as.numeric(nbrow)
      nbcol<<-as.numeric(nbcol)
      graphique()
     }
}



OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
#tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
#tkfocus(tt1)

}
####################################################
# Dessin Graphique
####################################################

graphique <- function()
{
    graphics.off()
# calcul des nouvelles coordonnées en fonction de l'angle de rotation theta
coords <- cbind(long,lat);
nvlecoord <- rotation(coords,theta);
nvlong <- nvlecoord[,1];
nvlat <- nvlecoord[,2];

# création de la fenêtre graphique
# x11(width=7, height=7);
nf <- layout(matrix(c(1,2,3,4),2,2,byrow=TRUE), c(1,1), c(1,1), TRUE)
layout.show(nf)

###########################################
# calcul des coordonnées de la grille
###########################################

h1 <- (max(nvlong)-min(nvlong))/nbcol;
h2 <- (max(nvlat)-min(nvlat))/nbrow;

# colonnes
xind <- min(nvlong);
milcol <- NULL;
for (i in 1:(nbcol-1))
{
    xind <- c(xind,min(nvlong)+h1*i);
    milcol <- c(milcol,(xind[i]+xind[i+1])/2);
}
xind <- c(xind,max(nvlong));
milcol <- c(milcol,(xind[nbcol]+xind[nbcol+1])/2);

# lignes
yind <- min(nvlat);
millig <- NULL;
for (j in 1:(nbrow-1))
{
    yind <- c(yind,min(nvlat)+h2*j);
    millig <- c(millig,(yind[j]+yind[j+1])/2);
}
yind <- c(yind,max(nvlat));
millig <- c(millig,(yind[nbrow]+yind[nbrow+1])/2);

###########################################
# calcul de la moyenne et de la médiane 
###########################################

# pour chaque colonne 
medcol <- NULL;
moycol <- NULL;
colvide <- rep(FALSE,nbcol);
for (i in 1:nbcol)
{
    if (i == nbcol)
    {
        if (xind[nbcol+1] > 0)
        {
            limmax <- 2*xind[nbcol+1];
        }
        else
        {
            limmax <- xind[nbcol+1]/2;
        }
    }
    else
    {
        limmax <- xind[i+1];
    }

    ens <- NULL;
    for (j in 1:length(nvlong))
    {
        if ((nvlong[j] >= xind[i]) && (nvlong[j] < limmax))
        {
            ens <- c(ens,var[j]);
        }
    }

    # si la colonne est vide
    if (length(ens) != 0)
    {
        moycol <- c(moycol,mean(ens))
        medcol <- c(medcol,median(ens))
    }
    else
    {
        colvide[i] <- TRUE;
        moycol <- c(moycol,0)
        medcol <- c(medcol,0)
    }
}

# pour chaque ligne
medlig <- NULL;
moylig <- NULL;
ligvide <- rep(FALSE,nbrow);
for (i in 1:nbrow)
{
    if (i == nbrow)
    {
        if (yind[nbrow+1] > 0)
        {
            limmax <- 2*yind[nbrow+1];
        }
        else
        {
            limmax <- yind[nbrow+1]/2;
        }
    }
    else
    {
        limmax <- yind[i+1];
    }

    ens <- NULL;
    for (j in 1:length(nvlong))
    {
        if ((nvlat[j] >= yind[i]) && (nvlat[j] < limmax))
        {
            ens <- c(ens,var[j]);
        }
    }

    # si la ligne est vide
    if (length(ens) != 0)
    {
        moylig <- c(moylig,mean(ens))
        medlig <- c(medlig,median(ens))
    }
    else
    {
        ligvide[i] <- TRUE;
        moylig <- c(moylig,0)
        medlig <- c(medlig,0)
    }
}

###########################################
# dessin de la carte 
###########################################

par(pty="s",mar=c(2,2,1,1))

# dessin des contours si ces derniers ont été précisés
if (length(carte) != 0)
{
    nvcarte <- rotation(carte,theta);
    n <- nrow(nvcarte);
    abs1 <- nvcarte[1:(n-1),1];
    ord1 <- nvcarte[1:(n-1),2];
    abs2 <- nvcarte[2:n,1];
    ord2 <- nvcarte[2:n,2];
#    limx <- c(min(nvcarte[,1]),max(nvcarte[,1]));
#    limy <- c(min(nvcarte[,2]), max(nvcarte[,2]));
    plot(nvlong,nvlat,"n", xlim=c(min(nvlong)-(max(nvlong)-min(nvlong))/10, max(nvlong)+(max(nvlong)-min(nvlong))/10), ylim=c(min(nvlat)-(max(nvlat)-min(nvlat))/10, max(nvlat)+(max(nvlat)-min(nvlat))/10),xlab=lablong,ylab=lablat);
    segments(abs1,ord1,abs2,ord2,col="blue")
    points(nvlong,nvlat,col="blue",pch=16, cex=0.5);
}
else
{
    plot(nvlong,nvlat,col="blue",pch=16,xlim=c(min(nvlong),max(nvlong)),ylim=c(min(nvlat),max(nvlat)),xlab=lablong,ylab=lablat);
}

# dessin de la grille
# colonnes
for (i in 1:(nbcol+1))
{
    segments(xind[i],min(nvlat),xind[i],max(nvlat),col="red");
}

# lignes
for (i in 1:(nbrow+1))
{
    segments(min(nvlong),yind[i],max(nvlong),yind[i],col="red");
}

if (interpol == 1)
{
    ligne <- "l";
}
else 
{
    ligne <- "p";
}

###########################################
# dessin des graphiques 
###########################################

# lignes
par(mar=c(2,0,1,1));

if (nuage == 0)
{
    plot(var,nvlat,"n",xlim=c(min(moylig[!ligvide],medlig[!ligvide]),max(moylig[!ligvide],medlig[!ligvide])),ylim=c(min(nvlat),max(nvlat)),yaxt="n");  
}
else
{
    plot(var,nvlat,col="pink",cex=0.8,xlim=c(min(var),max(var)),ylim=c(min(nvlat),max(nvlat)),yaxt="n");
}

if (symbol == 1)
{
    points(moylig[!ligvide],millig[!ligvide],col="blue",type=ligne,pch=10,lty=1);
    points(medlig[!ligvide],millig[!ligvide],col="red",type=ligne,pch=9,lty=2);
}
else
{
    points(moylig[!ligvide],millig[!ligvide],col="blue",type=ligne);
    points(medlig[!ligvide],millig[!ligvide],col="red",type=ligne);
}

# colonnes
par(mar=c(0,2,1,1))

if (nuage == 0)
{
    plot(nvlong,var,"n",xlim=c(min(nvlong),max(nvlong)),ylim=c(min(moycol[!colvide],medcol[!colvide]),max(moycol[!colvide],medcol[!colvide])),xaxt="n");
}
else
{
    plot(nvlong,var,col="pink",cex=0.8,xlim=c(min(nvlong),max(nvlong)),ylim=c(min(var),max(var)),xaxt="n");
}

if (symbol == 1)
{
    points(milcol[!colvide],moycol[!colvide],col="blue",type=ligne,pch=10,lty=1);
    points(milcol[!colvide],medcol[!colvide],col="red",type=ligne,pch=9,lty=2);
}
else
{
    points(milcol[!colvide],moycol[!colvide],col="blue",type=ligne);
    points(milcol[!colvide],medcol[!colvide],col="red",type=ligne);
}

###########################################
# précision des indications
###########################################

par(mar=c(0,0,1,1),xaxt="n",yaxt="n")

coordgraph <- cbind(c(0,0,-1,1,0.2),c(-1,1,0,0,1));
nvcoordgraph <- rotation (coordgraph,theta);

# dessin de la rose des vents
plot(-2:2,-2:2,"n",axes=FALSE);
text(nvcoordgraph[5,1],nvcoordgraph[5,2],"N", cex=0.8);
segments(nvcoordgraph[1,1],nvcoordgraph[1,2],nvcoordgraph[2,1],nvcoordgraph[2,2],col="black");
segments(nvcoordgraph[3,1],nvcoordgraph[3,2],nvcoordgraph[4,1],nvcoordgraph[4,2],col="black");

# légende
if (symbol == 1)
{
    if (interpol == 1)
    {
        pts <- " "
        lgn <- c(1,2)
    }
    else
    {
        pts <- c(10,9)
        lgn <- 0
    }
}
else
{
    if (interpol == 1)
    {
        pts <- " "
        lgn <- c(1,1)
    }
    else
    {
        pts <- c(16,16)
        lgn <- 0
    }
}

legend(-1.75,1.75, c("Mean","Median"), col=c("blue","red"), pch=pts,lty=lgn, cex=1)

# nom de la variable étudiée
if (labvar != "")
{
    msg <- paste("Variable : ",labvar, sep="")
    text(0,-1.75, msg, col="black")
}



    }


####################################################
# quitter l'application
####################################################

    quitfunc <- function() {
        tclvalue(fin) <<- TRUE
        graphics.off()
        tkdestroy(tt)

   #    tkdestroy(tt1)
   #tkwait.variable(fin)
    }

graphique()
####################################################
# création de la boite de dialogue
####################################################
if(interactive())
{
tt <- tktoplevel()


labelText12 <- tclVar("Change the number of cells")
label12 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText12))
tkconfigure(label12, textvariable=labelText12)
tkgrid(label12,columnspan=2)


vari12.but <- tkbutton(tt, text="     OK     ", command=choixgrid);
tkgrid(vari12.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

labelText1 <- tclVar("Change the rotation angle")
label1 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)


vari1.but <- tkbutton(tt, text="     OK     ", command=choixangle);
tkgrid(vari1.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText5 <- tclVar("Exit")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))
####################################################

tkwait.variable(fin)
}

}

