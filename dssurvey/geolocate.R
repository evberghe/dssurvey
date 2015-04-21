# after https://heuristically.wordpress.com/2013/05/20/geolocate-ip-addresses-in-r/
# changed the function to get rid of the 'rbind'-style growing of an object
# cleared some more problems, with data structures
#
# TODO: error handling as in http://stackoverflow.com/questions/17536221/writing-a-function-to-separate-working-vs-error-values-in-r
# TODO: look for alternatives to freegeoip (doesn't seem to be very accurate);
#		check links in http://www.quora.com/What-are-the-best-products-or-tools-that-convert-an-IP-address-or-postal-code-to-a-location-(lat-long-and-or-city)
###############################################################################


library("rjson")

geoip <- function(ip, format = ifelse(length(ip)==1,"list","dataframe"))
{
	if (length(ip)==1)
	{
		# a single IP address
		url <- paste("http://freegeoip.net/json/", ip, sep="")
		ret <- fromJSON(readLines(url, warn=FALSE))
		if (format=="dataframe") ret <- as.data.frame(ret)
	} else {
		for (i in 1:length(ip))
		{
			r <- geoip(ip[i], format="dataframe")
			r[sapply(r, is.factor)] <- lapply(r[sapply(r, is.factor)], as.character)
			if(i==1){
				# initialise data frame with i rows, columns from the first row we just got
				ret <- matrix(rep(NA, length(r)*length(ip)), nrow=length(ip))
				ret <- as.data.frame(ret)
				names(ret) <- names(r)
			}
			ret[i, ] <- r # if we set stringsAsFactors==TRUE, we get integers into ret instead of factors or strings
		}
	}
	if(format=="dataframe")
		ret[sapply(ret, is.character)] <- lapply(ret[sapply(ret, is.character)], as.factor) #back to factors
	return(ret)
}   

##############################################################################################
# testing
##fname <- "satisfaction_150406.zip"
##setwd("satsurvey")
##unzip(paste("data", fname, sep="/"), "CSV/Sheet_1.csv", junkpaths=TRUE, setTimes=TRUE)
##mydata <- read.table("Sheet_1.csv", header=TRUE, sep=",")
##file.remove("Sheet_1.csv")

##myIPs <- as.character(mydata[, "IP.Address"])
# returns a proper data frame, with two of the columns being latitude and longitude
# r <- geoip(myIPs, "dataframe")
