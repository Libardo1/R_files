complete <- function(directory, id=1:332){
	required = data.frame(id,nobs=id)
	main = 1
	for (x in id)
	{
		x = strtoi(x)
		if (x < 10)
		{
			loc = paste(directory,"/","00",x,".csv",sep='')
		}
		else if (x >= 10 && x < 100)
		{
			loc = paste(directory,"/","0",x,".csv",sep='')
		}
		else
		{
			loc = paste(directory,"/",x,".csv",sep='')
		}
		data = as.data.frame(read.csv(loc))
		count = 0
		for (y in 1:nrow(data))
		{
			if (is.na(data$sulfate[y]) | is.na(data$nitrate[y]))
			{
				count = count + 1
			}
		}
		required$nobs[main] = nrow(data) - count
		main = main  + 1
		
	}
	required
}