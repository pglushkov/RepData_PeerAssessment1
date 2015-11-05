run_proc <- function() {
	DATA <- read.csv(unz("activity.zip", "activity.csv"))
	#head(DATA, 5);
	
	days = split(DATA, DATA$date);
	steps_per_day = unlist(lapply(days, steps_per_day_fun));
	
	# Plotting histogram of total steps per day
	windows();
	hist(steps_per_day, main = 'Histogram of total steps per day', 
		xlab = 'total steps per day', ylab = 'number of occurences');
	
	print(sprintf("Mean steps per day : %f", mean(steps_per_day)));
	print(sprintf("Median steps per day : %f", median(steps_per_day)));
	
	avg_by_interval = vector(mode = 'numeric', length=length(days[[1]]$interval) );
	for (k in 1:length(avg_by_interval)) {
		avg_by_interval[k] = mean(DATA[DATA$interval == (k-1)*5 , 'steps'], na.rm = TRUE);
	}
	
	avg_by_interval[is.nan(avg_by_interval)] = 0;

	interval_ids = days[[1]]$interval;
	# Plotting average number of steps per interval, calculated across all days
	windows();
	plot(avg_by_interval ~ interval_ids, type = 'n', main = 'Average number of steps per interval',
		xlab = 'Interval ID', ylab = 'Average number of steps');
	lines(avg_by_interval ~ interval_ids, col = 'black');

	max_interval_id = interval_ids[which.max(avg_by_interval)];
	print( sprintf("Interval with biggest average number of steps: %d",max_interval_id) );

	#### finding number of missing values
	num_missing = sum(is.na(DATA$steps));
	print( sprintf("Total number of missing values: %d", num_missing) );

	values_fill_map = rep(avg_by_interval, length(days));
	values_fill_map = values_fill_map * as.numeric(is.na(DATA$steps));
	DATA$steps[is.na(DATA$steps)] = 0;
	DATA$steps <- DATA$steps + values_fill_map;

	days = split(DATA, DATA$date);
	steps_per_day = unlist(lapply(days, steps_per_day_fun));
	
	# Plotting histogram of total steps per day, calculated on data with filled missing values
	windows();
	hist(steps_per_day, main = 'Histogram of total steps per day (Missed values filled)', 
		xlab = 'total steps per day', ylab = 'number of occurences');
	print(sprintf("Mean steps per day : %f (after filling missing values)", mean(steps_per_day)));
	print(sprintf("Median steps per day : %f (after filling missing values)", median(steps_per_day)));

	weekend_days = c('Saturday', 'Sunday');
	weekday = weekdays(as.Date(DATA$date));
	weekday = factor(weekday %in% weekend_days, levels = c(TRUE, FALSE), labels = c('weekend','workday'));
	DATA <- cbind(DATA, weekday );
	
	WD_DATA = DATA[DATA$weekday == 'workday',];
	WE_DATA = DATA[DATA$weekday == 'weekend',];
	
	wd_avg_by_interval = vector(mode = 'numeric', length=length(days[[1]]$interval) );
	for (k in 1:length(wd_avg_by_interval)) {
		wd_avg_by_interval[k] = mean(WD_DATA[DATA$interval == (k-1)*5 , 'steps'], na.rm = TRUE);
	}
	wd_avg_by_interval[is.nan(wd_avg_by_interval)] = 0;

	we_avg_by_interval = vector(mode = 'numeric', length=length(days[[1]]$interval) );
	for (k in 1:length(we_avg_by_interval)) {
		we_avg_by_interval[k] = mean(WE_DATA[DATA$interval == (k-1)*5 , 'steps'], na.rm = TRUE);
	}
	we_avg_by_interval[is.nan(we_avg_by_interval)] = 0;

	# Plotting average number of steps per interval, calucalted separately accross workdays and weekends
	windows();
	par(mfrow = c(2,1));
	plot(wd_avg_by_interval ~ interval_ids, main = 'on workdays', type="n",
		xlab = 'Interval ID', ylab = 'Average number of steps');
	lines(wd_avg_by_interval ~ interval_ids, col = 'blue');
	plot(we_avg_by_interval ~ interval_ids, main = 'on weekends', type="n",
		xlab = 'Interval ID', ylab = 'Average number of steps');
	lines(we_avg_by_interval ~ interval_ids, col = 'blue');
}

steps_per_day_fun <- function (daydata) {
	
	steps = as.vector(daydata$steps);
	return( sum(steps, na.rm = TRUE) );
}

