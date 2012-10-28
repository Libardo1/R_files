getmonitor <- function (id, directory, summarize = FALSE) {
	id = strtoi(id)
	if (id < 10)
	{
		loc = paste(directory,"/","00",id,".csv",sep='')
	}
	else if (id >= 10 && id < 100)
	{
		loc = paste(directory,"/","0",id,".csv",sep='')
	}
	else
	{
		loc = paste(directory,"/",id,".csv",sep='')
	}
	if (summarize == FALSE)
	{
		as.data.frame(read.csv(loc))
	}
	else
	{
		print (read.csv(loc))
		invisible(read.csv(loc))
	}
}