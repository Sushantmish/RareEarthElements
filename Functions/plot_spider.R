#this is a function for plotting spider plots for REE data
plot_spider <- function(sample_name,reference_values,logscale=TRUE,colors=NULL,main= "Spider Plot for Normalised REE Values",xlab="Rare Earth Elements",ylab ="Normalised values")
{
  # these are the REE we will be covering generally
  REE_symbols <-  c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")
  # normalising values of samples with the reference set
  normalised_values <- lapply(sample_name,function(s1) as.numeric(s1)/as.numeric(reference_values))
  #axis range
  ymin <- if (logscale) 1 else 0
  ymax <-  max(unlist(normalised_values))
  # generate plot 
  plot(1:length(reference_values),normalised_values[[1]],type="b",xaxt="n",,xlab="Rare Earth Elements",ylab ="Concentration in ppm",log = 'y')
  axis(1,at=1:length(reference_values),labels = REE_symbols)
  grid(nx=NULL, ny=NULL,col="gray",lwd=1)
  abline(h=1,col="black",lwd=3)
  
  # generate other plots 
   if(length(normalised_values)>1)
   { 
       for(i in 2:length(normalised_values))
       {
         lines(1:length(normalised_values),normalised_values[[i]],type="b")
       }
   }
}
