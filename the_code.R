run_proc <- function() {
	DATA <- read.csv(unz("activity.zip", "activity.csv"))
	#head(DATA, 5);
	
	days = split(DATA, DATA$date);
	steps_per_day = unlist(lapply(days, steps_per_day_fun));
	
	hist(steps_per_day);
	
	print(sprintf("Mean steps per day : %f", mean(steps_per_day)));
	print(sprintf("Median steps per day : %f", median(steps_per_day)));
	
	avg_by_interval = vector(mode = 'numeric', length=length(days[[1]]$interval) );
	for (k in 1:length(avg_by_interval)) {
		avg_by_interval[k] = mean(DATA[DATA$interval == (k-1)*5 , 'steps'], na.rm = TRUE);
	}
	
	interval_ids = days[[1]]$interval;
	plot(avg_by_interval ~ interval_ids);
	
	max_interval_id = interval_ids[which.max(avg_by_interval)];
	print( sprintf("Interval with biggest average number of steps: %d",max_interval_id) );
}

steps_per_day_fun <- function (daydata) {
	
	steps = as.vector(daydata$steps);
	return( sum(steps, na.rm = TRUE) );
}