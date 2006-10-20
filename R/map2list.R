"map2list" <-
function(data)
{
if(class(data)[1]=="Map")
 { n<-length(data[1][[1]])

 if(length(data[1][[1]][1][[1]]$verts)==2)
  {tkmessageBox(message="Return spatial points pattern only",icon="warning",type="ok")

   X<- (data[1][[1]][1][[1]]$bbox[1]+data[1][[1]][1][[1]]$bbox[3])/2
   Y<- (data[1][[1]][1][[1]]$bbox[2]+data[1][[1]][1][[1]]$bbox[4])/2

   for(i in 1:(n-1))
    {
    X<-rbind(X,(data[1][[1]][i+1][[1]]$bbox[1]+data[1][[1]][i+1][[1]]$bbox[3])/2)
    Y<-rbind(Y,(data[1][[1]][i+1][[1]]$bbox[2]+data[1][[1]][i+1][[1]]$bbox[4])/2)
    }

   return(list(X=X,Y=Y))
  }
 else
 { tkmessageBox(message="Return spatial points patterns and polygons pattern",icon="warning",type="ok")
  
   contours<-rbind(NA,NA,NA,data[1][[1]][1][[1]]$verts)
   X<- (data[1][[1]][1][[1]]$bbox[1]+data[1][[1]][1][[1]]$bbox[3])/2
   Y<- (data[1][[1]][1][[1]]$bbox[2]+data[1][[1]][1][[1]]$bbox[4])/2

  for(i in 1:(n-1))
   {
    contours=rbind(contours,NA,NA,NA,data[1][[1]][i+1][[1]]$verts)   
    X<-rbind(X,(data[1][[1]][i+1][[1]]$bbox[1]+data[1][[1]][i+1][[1]]$bbox[3])/2)
    Y<-rbind(Y,(data[1][[1]][i+1][[1]]$bbox[2]+data[1][[1]][i+1][[1]]$bbox[4])/2)
   }

 contours=rbind(contours,NA,NA,NA)
  return(list(X=X,Y=Y,poly=contours))
 }
}
else
 {
  tkmessageBox(message="Must be a Map object",icon="warning",type="ok")
 }
 

}

