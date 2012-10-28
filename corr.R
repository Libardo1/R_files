corr <- function(directory, threshold = 0) {
      main = 1
	answer = rep (NA,332)
	for (x in 1:332)
	{
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
		sulfate = rep(NA,nrow(data))
		nitrate = rep(NA,nrow(data))
		for (y in 1:nrow(data))
		{
			if (is.na(data$sulfate[y]) | is.na(data$nitrate[y]))
			{
				count = count + 1
			}
			else
			{
				sulfate[y] = data$sulfate[y]
				nitrate[y] = data$nitrate[y]
			}
			
		}
		if ((nrow(data) - count) >= threshold)
		{
			answer[main] = cor (na.omit(sulfate),na.omit(nitrate))
		}
		main = main + 1
		
	}
	na.omit(answer)

}