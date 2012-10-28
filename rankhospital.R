rankhospital <- function(state, outcome, num)
{
	main <- read.csv("c:/users/owner/desktop/cfda/assgn2data/outcome-of-care-measures.csv")
	data <- data.frame(names = main$"Hospital.Name",states = main$"State",HeartAttack = main$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",HeartFailure = main$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", Pneumonia =main$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
	flag = 0
	for (x in data$states)
	{
		if (state == as.character(x))
		{
			flag = 1
			break
		}
	}
	if (flag == 1)
	{
		if (outcome == "heart attack" | outcome == "pneumonia" | outcome == "heart failure")
		{
			w = 9
		}
		else
		{
			stop("invalid outcome")
		}
	}
	else
	{
		stop("invalid state")
	}
	if (outcome == "heart attack")
	{
		table = as.vector(data$HeartAttack,mode='character')
	}
	else if (outcome == "heart failure")
	{
		table = as.vector(data$HeartFailure,mode='character')
	}
	else
	{
		table = as.vector(data$Pneumonia,mode='character')
	}
	noofna = 0
	scount = 0
	for (x in 1:nrow(data))
	{
		if (state == as.character(data$states[x]))
		{
			scount = scount + 1
		}
	}
	ref = 1:scount
	req = data.frame(names = ref,values = 1:scount)
	scount = 1
	for (x in 1:nrow(data))
	{
		if (state == as.character(data$states[x]))
		{
			if (is.na(as.real(table[x])))
			{
				noofna = noofna + 1
				req$names[scount] = as.character(data$names[x])
				req$values[scount] = 1000
				scount = scount + 1
				
			}
			else
			{
				req$names[scount] = as.character(data$names[x])
				req$values[scount] = as.real(table[x])
				scount = scount + 1			
			}
		}
	}
	scount = scount - 1
	if (num == "best")
	{
		num = 1
	}
	else if (num == "worst")
	{
		num = scount - noofna
	}
	if (num > scount - noofna)
	{
		NA
	}
	req = req[order(req$values,req$names),]
	req$names[num]
}
		