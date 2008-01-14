`polylist2list` <-
function(data)
{
if(class(data)[1]=="polylist")
 {n<-length(data[])
 contours<-rbind(NA,NA,NA,data[[1]][,])

  for(i in 1:(n-1))
   {
    contours=rbind(contours,NA,NA,NA,data[[i+1]][,])   
   }

 contours=rbind(contours,NA,NA,NA)
 return(contours)
 }
else
 {
  tkmessageBox(message="Must be a polylist object",icon="warning",type="ok")
 }


}

