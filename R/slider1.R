`slider1` <-
function(fenetre,refresh.code,names,minima,maxima,resolutions,starts,no=0)
{
    if (no != 0) 
    {
        return(as.numeric(tclvalue(get(paste("slider",no,sep=""),env=slidenv))))
    }

#    if (set.no.value[1] != 0)
#    { 
#       try(eval(parse(text=paste("tclvalue(slider",set.no.value[1],")<-",set.no.value[2],sep="")),env=slidenv)); 
#       return(set.no.value[2]) 
#   }

    if (!exists("slidenv"))
    { 
        slidenv<-new.env()
    }

    for(i in seq(names))
    {
        eval(parse(text=paste("assign(\"slider",i,"\",tclVar(starts[i]),env=slidenv)",sep="")))
    }

    for(i in seq(names))
    {
        lab<-tklabel(fenetre, text=names[i], width="25")
        sc<-tkscale(fenetre, command=refresh.code, from=minima[i], to=maxima[i],showvalue=TRUE, resolution=resolutions[i], orient="horiz") 
        assign("sc",sc,env=slidenv); 
        tkgrid(lab,sc)
        eval(parse(text=paste("tkconfigure(sc,variable=slider",i,")",sep="")),env=slidenv);
    }
  }

