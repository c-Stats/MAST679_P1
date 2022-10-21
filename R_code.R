require("data.table")
require("magrittr")
require("dplyr")
require("ggplot2")
require("stringr")
require("xgboost")
require("tidymodels")
require("recipes")
require("doParallel")



directory <- "C:/Users/Frank/OneDrive/Documents/git/MAST679_P1"

#Load data
all_data <- list()
for(i in 1:2){

	all_data[[length(all_data) + 1]] <- list()
	names(all_data)[i] <- paste("game", i, sep = "_")

	all_data[[i]] <- list(raw = paste(directory, "/data/Sample_Game_", i, "/Sample_Game_", i, "_RawEventsData.csv", sep = ""),
							away = paste(directory, "/data/Sample_Game_", i, "/Sample_Game_", i, "_RawTrackingData_Away_Team.csv", sep = ""),
							home = paste(directory, "/data/Sample_Game_", i, "/Sample_Game_", i, "_RawTrackingData_Home_Team.csv", sep = "")
							)

	all_data[[i]] <- sapply(all_data[[i]], function(x){fread(x)})

	names(all_data[[i]][[1]]) <- sapply(names(all_data[[i]][[1]]), function(x){str_replace(x, " ", "_")})
	
	for(j in 2:3){

		#Get proper names for columns since the csv is messed up
		cnames <- unlist(all_data[[i]][[j]][3])
		is_na <- which(is.na(cnames))
		cnames[is_na] <- paste(cnames[is_na - 1], "_y", sep = "")
		cnames[is_na - 1] <- paste(cnames[is_na - 1], "_x", sep = "")
		cnames[3] <- "Time"

		all_data[[i]][[j]] <- all_data[[i]][[j]][-c(1:3)]
		names(all_data[[i]][[j]]) <- cnames

		all_data[[i]][[j]][, Time := as.numeric(Time)]

		to_convert <- names(all_data[[i]][[j]])[-c(1:2)]
		all_data[[i]][[j]][, (to_convert) := lapply(.SD, as.numeric), .SDcols = to_convert]

	}

}

#Choose first match
data <- data.table::copy(all_data$game_1$raw)
home <- data.table::copy(all_data$game_1$home)
away <- data.table::copy(all_data$game_1$away)


#Types for shots on goals
data[Type == "SHOT", SoG_Type := sapply(Subtype, function(col){

		if(any(sapply(c("GOAL", "SAVED", "OUT"), function(x){grepl(x, col, TRUE)}))){

			out <- str_split(col, "-")[[1]]
			out[length(out)]

		}  else {

			col

		}

	})]


#Tag sequences before each shot on goal (SoG)
#Check on which side the home team goal is on
SoG_index <- data[!is.na(SoG_Type), which = TRUE]
intervals <- c(1, SoG_index)

for(i in c(1:(length(intervals) - 1))){

	index <- c(intervals[i]:(intervals[i+1] - 1))
	data[index, Sequence := i]

	shot <- data[index[length(index)]+1] 
	team <- shot$Team 
	goal_position <- shot$End_X
	if(abs(1 - goal_position) < abs(goal_position)){

		goal_position <- 1

	} else {

		goal_position <- 0

	}

	if(team == "home"){

		home_goal_at <- abs(1 - goal_position)

	} else {

		home_goal_at <- goal_position

	} 

	data[index, Home_goal_x := home_goal_at]

}

data[c(intervals[length(intervals)]:nrow(data)), Sequence := length(SoG_index) + 1]
data[c(intervals[length(intervals)]:nrow(data)), Home_goal_x := data[index[1], "Home_goal_x"]$Home_goal_x]

#Tag the x positions of the goal for Type == PASS
data[Type == "PASS" & Team == "Away" & Home_goal_x == 0, Goal_x := 0] %>%
	.[Type == "PASS" & Team == "Away" & Home_goal_x == 1, Goal_x := 1] %>%
	.[Type == "PASS" & Team == "Home" & Home_goal_x == 0, Goal_x := 1] %>%
	.[Type == "PASS" & Team == "Home" & Home_goal_x == 1, Goal_x := 0] 

#Add a primaty key for rows
data[, Key := c(1:nrow(data))]

#Use Time as keys for the home and away frames
setkey(home, "Time")
setkey(away, "Time")

#Function to analyse pass


pass_data <- function(n, data = data, home = home, away = away){

	#Make sure home and away are keyed for fast lookups
	if(is.null(key(home)) | key(home) != "Time"){

		setkey(home, "Time")

	}

	if(is.null(key(away)) | key(away) != "Time"){

		setkey(away, "Time")

	}

	#Extract relevant row data
	data_row <- data.table::copy(data[n])
	if(data_row$Team == "Home"){

		team_row_0 <- data.table::copy(home[.(data_row$`Start_Time [s]`)])
		opponent_row_0 <- data.table::copy(away[.(data_row$`Start_Time [s]`)])
		team_row_1 <- data.table::copy(home[.(data_row$`End_Time [s]`)])
		opponent_row_1 <- data.table::copy(away[.(data_row$`End_Time [s]`)])		

	} else {

		team_row_0 <- data.table::copy(away[.(data_row$`Start_Time [s]`)])
		opponent_row_0 <- data.table::copy(home[.(data_row$`Start_Time [s]`)])
		team_row_1 <- data.table::copy(away[.(data_row$`End_Time [s]`)])
		opponent_row_1 <- data.table::copy(home[.(data_row$`End_Time [s]`)])		

	}

	keep <- names(which(sapply(names(team_row_0), function(x){grepl("Player", x, TRUE) & grepl("_", x, TRUE)})))
	team_row_0 <- team_row_0[, keep, with = FALSE]
	team_row_1 <- team_row_1[, keep, with = FALSE]
	
	keep <- names(which(sapply(names(opponent_row_0), function(x){grepl("Player", x, TRUE) & grepl("_", x, TRUE)})))
	opponent_row_0 <- opponent_row_0[, keep, with = FALSE]
	opponent_row_1 <- opponent_row_1[, keep, with = FALSE]


	#If the goal is sitting at (0, 0.5), then rotate the coordinates by 1pi radian with centre (0.5, 0.5)
	#I.e.: make the statistics invariant with respect to which side the targeted goal is on
	if(data_row$Goal_x == 0){

		data_row[, c("Start_X", "Start_Y", "End_X", "End_Y", "Goal_x") := lapply(.SD, function(x){1 - x}), .SDcols = c("Start_X", "Start_Y", "End_X", "End_Y", "Goal_x")]
		
		team_player_cols <- names(which(sapply(names(team_row_0), function(x){ grepl("Player", x, TRUE)})))
		opponent_player_cols <- names(which(sapply(names(opponent_row_0), function(x){grepl("Player", x, TRUE)})))

		team_row_0[, names(team_row_0) := lapply(.SD, function(x){1 - x}), .SDcols = names(team_row_0)]
		team_row_1[, names(team_row_1) := lapply(.SD, function(x){1 - x}), .SDcols = names(team_row_1)]

		opponent_row_0[, names(opponent_row_0) := lapply(.SD, function(x){1 - x}), .SDcols = names(opponent_row_0)]
		opponent_row_1[, names(opponent_row_1) := lapply(.SD, function(x){1 - x}), .SDcols = names(opponent_row_1)]

	}

	#Split player positions by x and y coordinates
	positions_0 <- list()
	positions_0$team <- list(x = team_row_0[, names(which(sapply(names(team_row_0), function(x){grepl("_x", x, TRUE)}))), with = FALSE],
							y = team_row_0[, names(which(sapply(names(team_row_0), function(x){grepl("_y", x, TRUE)}))), with = FALSE])
	positions_0$opponent <- list(x = opponent_row_0[, names(which(sapply(names(opponent_row_0), function(x){grepl("_x", x, TRUE)}))), with = FALSE],
							y = opponent_row_0[, names(which(sapply(names(opponent_row_0), function(x){grepl("_y", x, TRUE)}))), with = FALSE])

	positions_0$team <- lapply(positions_0$team, unlist)
	positions_0$opponent <- lapply(positions_0$opponent, unlist)
	positions_0$team <- lapply(positions_0$team, function(x){x[!is.na(x)]})
	positions_0$opponent <- lapply(positions_0$opponent, function(x){x[!is.na(x)]})	

	#Split player positions by x and y coordinates
	positions_1 <- list()
	positions_1$team <- list(x = team_row_1[, names(which(sapply(names(team_row_1), function(x){grepl("_x", x, TRUE)}))), with = FALSE],
							y = team_row_1[, names(which(sapply(names(team_row_1), function(x){grepl("_y", x, TRUE)}))), with = FALSE])
	positions_1$opponent <- list(x = opponent_row_1[, names(which(sapply(names(opponent_row_1), function(x){grepl("_x", x, TRUE)}))), with = FALSE],
							y = opponent_row_1[, names(which(sapply(names(opponent_row_1), function(x){grepl("_y", x, TRUE)}))), with = FALSE])

	positions_1$team <- lapply(positions_1$team, unlist)
	positions_1$opponent <- lapply(positions_1$opponent, unlist)
	positions_1$team <- lapply(positions_1$team, function(x){x[!is.na(x)]})
	positions_1$opponent <- lapply(positions_1$opponent, function(x){x[!is.na(x)]})	


	#Position of targeted goal and ball (time 0 and time 1)
	goal_position <- c(1, 0.5)
	ball_0 <- unlist(data_row[, c("Start_X", "Start_Y")])
	ball_1 <- unlist(data_row[, c("End_X", "End_Y")])

	#Ball and goal distance statistics
	ball_distance_traveled <- sqrt(sum((ball_1 - ball_0)^2))	
	
	d_0 <- sqrt(sum((goal_position - ball_0)^2))
	d_1 <- sqrt(sum((goal_position - ball_1)^2))
	d_delta <- d_1 - d_0 
	d_delta_percent <- d_delta / d_0

	#Angle from ball to goal
	angle_0 <- abs(acos(sum(ball_0 * goal_position) / (sqrt(sum(ball_0^2)) * sqrt(sum(goal_position^2)))))
	if(!is.na(angle_0)){

		if(angle_0 > pi){

			angle_0 <- 2*pi - angle_0

		}


	}

	angle_1 <- abs(acos(sum(ball_1 * goal_position) / (sqrt(sum(ball_1^2)) * sqrt(sum(goal_position^2)))))
	if(!is.na(angle_1)){

		if(angle_1 > pi){

			angle_1 <- 2*pi - angle_1

		}


	}


	#Is the ball on the right side
	ball_right_0 <- ball_0[2] < 0.5
	ball_right_1 <- ball_1[2] < 0.5

	#Players behind ball at t=0 vs t=1
	team_behind_ball_0 <- length(which(positions_0$team$x < ball_0[1]))
	team_behind_ball_1 <- length(which(positions_1$team$x < ball_1[1]))

	opponent_behind_ball_0 <- length(which(positions_0$opponent$x < ball_0[1]))
	opponent_behind_ball_1 <- length(which(positions_1$opponent$x < ball_1[1]))	

	#Players ahead of ball at t=0 vs t=1
	team_ahead_ball_0 <- 11 - team_behind_ball_0
	team_ahead_ball_1 <- 11 - team_behind_ball_1

	opponent_ahead_ball_0 <- 11 - opponent_behind_ball_0
	opponent_ahead_ball_1 <- 11 - opponent_behind_ball_1

	#Change in number of players ahead
	team_ahead_delta <- team_ahead_ball_1 - team_ahead_ball_0 
	opponent_ahead_delta <- opponent_ahead_ball_1 - opponent_ahead_ball_0

	#Player distance from ball before pass
	distance_from_ball_team_0 <- sqrt((positions_0$team$x - ball_1[1])^2 + (positions_0$team$y - ball_0[2])^2)
	distance_from_ball_opponent_0 <- sqrt((positions_0$opponent$x - ball_1[2])^2 + (positions_0$opponent$y - ball_0[2])^2)

	#Player distance from ball after pass
	distance_from_ball_team_1 <- sqrt((positions_1$team$x - ball_1[1])^2 + (positions_1$team$y - ball_1[2])^2)
	distance_from_ball_opponent_1 <- sqrt((positions_1$opponent$x - ball_1[2])^2 + (positions_1$opponent$y - ball_1[2])^2)

	#Mean distance of the players (from the ball) at t=0 and t=1, excluding the one with the ball
	d_team_0 <- mean(distance_from_ball_team_0[-which.min(distance_from_ball_team_0)])
	d_opponent_0 <- mean(distance_from_ball_opponent_0)

	d_team_1 <- mean(distance_from_ball_team_1[-which.min(distance_from_ball_team_1)])
	d_opponent_1 <- mean(distance_from_ball_opponent_1)	

	#Mean distance of the 3 closest players (from the ball) at t=0 and t=1, excluding the one with the ball
	top_3_d_team_0 <- mean(head(sort(distance_from_ball_team_0), 4)[-1])
	top_3_d_opponent_0 <- mean(head(sort(distance_from_ball_opponent_0), 3))

	top_3_d_team_1 <- mean(head(sort(distance_from_ball_team_1), 4)[-1])
	top_3_d_opponent_1 <- mean(head(sort(distance_from_ball_opponent_1), 3))

	#Deltas for distance
	d_team_delta <- d_team_1 - d_team_0
	d_opponent_delta <- d_opponent_1 - d_opponent_0 

	top_3_d_team_delta <- top_3_d_team_1 - top_3_d_team_0
	top_3_d_opponent_delta <- top_3_d_opponent_1 - top_3_d_opponent_0 	


	#Output
	out <- c(ball_distance_traveled, 
				d_0, d_1, d_delta, d_delta_percent,
				angle_0, angle_1, ball_right_0, ball_right_1,
				team_ahead_ball_0, team_ahead_ball_1, opponent_ahead_ball_0, opponent_ahead_ball_1,
				team_ahead_delta, opponent_ahead_delta,
				d_team_1, d_opponent_1, top_3_d_team_1, top_3_d_opponent_1,
				d_team_delta, top_3_d_opponent_delta)

	out <- sapply(out, function(x){round(x, 2)})
	out <- as.data.table(t(out))

	names(out) <- c("distance_traveled",
					"DfG_0", "DfG_1", "DfG_delta", "DfG_delta_p",
					"angle_0", "angle_1", "ball_right_0", "ball_right_1",
					"team_ahead_ball_0", "team_ahead_ball_1", "opponent_ahead_ball_0", "opponent_ahead_ball_1",
					"team_ahead_delta", "opponent_ahead_delta",
					"d_team_1", "d_opponent_1", "top_3_d_team_1", "top_3_d_opponent_1",
					"d_team_delta", "top_3_d_opponent_delta")

	return(cbind(data_row, out))

}



#Build dataframe with passes
pass_index <- data[Type == "PASS", which = TRUE]
data_pass <- lapply(as.list(pass_index), function(n){pass_data(n, data = data, home = home, away = away)})
names(data_pass) <- c(1:length(data_pass))

data_pass <- data.table::copy(dplyr::bind_rows(Filter(Negate(is.null), data_pass)))

#Distribution of the points according to the proposed metric
pass_distribution <- data_pass[, lapply(.SD, function(x){

							n <- sapply(x, function(x){if(x >= 0){0} else {abs(x)}})
							values <- sort(unique(n))
							max_val <- max(values)

							out <- c(0:max_val)
							for(i in 1:length(out)){

								out[i] <- length(which(x == -out[i]))

							}

							return(out)


					}), by = c("Team"), .SDcols = c("opponent_ahead_delta")] %>%
					.[, Pass_score := lapply(.SD, function(x){c(0:(length(x) - 1))}), by = "Team", .SDcols = c("opponent_ahead_delta")]

pass_distribution <- pass_distribution[, c("Team", "Pass_score", "opponent_ahead_delta"), with = FALSE]
names(pass_distribution) <- c("Team", "Pass_score", "n")


barcharts_pass_score <- list(

	away = 	ggplot(data = pass_distribution[Team == "Away"], aes(x = Pass_score, y = n)) +
			geom_bar(stat = "identity", fill = "steelblue") +
			theme_minimal() +
			xlab("Pass score") +
			ylab("Number of passes") +
			ggtitle("Away"),

	home =  ggplot(data = pass_distribution[Team == "Home"], aes(x = Pass_score, y = n)) +
			geom_bar(stat = "identity", fill = "steelblue") +
			theme_minimal() +
			xlab("Pass score") +
			ylab("Number of passes") +
			ggtitle("Home")

)


#Build the frame for xgboost
subsequent_play_key <- data_pass$Key + 1
outcome <- data[subsequent_play_key, c("Team", "Type", "Subtype")]

columns <- c("distance_traveled",
					"DfG_0", "DfG_1", "DfG_delta", "DfG_delta_p",
					"angle_0", "angle_1", "ball_right_0", "ball_right_1",
					"team_ahead_ball_0", "team_ahead_ball_1", "opponent_ahead_ball_0", "opponent_ahead_ball_1",
					"team_ahead_delta", "opponent_ahead_delta",
					"d_team_1", "d_opponent_1", "top_3_d_team_1", "top_3_d_opponent_1",
					"d_team_delta", "top_3_d_opponent_delta", "Team")

X <- data_pass[, columns, with = FALSE]
names(X)[ncol(X)] <- "Team_0"

names(outcome)[1] <- "Team_1"
X <- cbind(X, outcome)

#Possible outcomes
possible_outcomes <- unique(X[, c("Type", "Subtype"), with = FALSE])
possible_outcomes <- possible_outcomes[order(Type, Subtype)]

#Is the subsequent play made by the same team?
X[, Same_Team := (Team_0 == Team_1)]

#Tag the outcomes
X[Same_Team == TRUE & Type == "PASS", Next_Event := "PASS_FRIENDLY"] %>%
.[Same_Team == FALSE & Type == "PASS", Next_Event := "PASS_ENEMY"] %>%
.[Type == "BALL LOST", Next_Event := "BALL LOST"] %>%
.[Type == "BALL OUT", Next_Event := "BALL OUT"] %>%
.[Type == "CHALLENGE" & grepl("LOST", Subtype, TRUE), Next_Event := "CHALLENGE_LOST"] %>%
.[Type == "CHALLENGE" & grepl("WON", Subtype, TRUE), Next_Event := "CHALLENGE_WON"] %>%
.[Subtype == "INTERCEPTION", Next_Event := "INTERCEPTION"] %>%
.[Type == "SHOT", Next_Event := "SHOT"] %>%
.[, Next_Event := as.factor(Next_Event)]

#Final frame
X <- X[, c(columns[-length(columns)], "Next_Event"), with = FALSE]


#Folds
folds <- vfold_cv(X, v = 10)

#Recipe
rcp <- recipe(Next_Event ~ ., data = X) %>% prep()

#Model
xgb_model <- boost_tree(trees = tune(), learn_rate = tune(),
					    tree_depth = tune(), min_n = tune(),
                        loss_reduction = tune(), 
                        sample_size = tune(), mtry = tune()) %>% 
             set_mode("classification") %>% 
             set_engine("xgboost", nthread = 16)

#Parameters
xgboost_params <- parameters(trees(), learn_rate(),
                             tree_depth(), min_n(), 
                             loss_reduction(),
                             sample_size = sample_prop(), finalize(mtry(), X)) 

xgboost_params <- xgboost_params %>% update(trees = trees(c(100, 150)))             

#Workflow
wflow <- workflow() %>%
	     add_model(xgb_model) %>%
	     add_recipe(rcp)

#Train model
doParallel::registerDoParallel(16)
xgboost_model <- wflow %>%
                 tune_bayes(resamples = folds,
                            param_info = xgboost_params,
                            initial = 10,
                            iter = 50, 
                            metrics = metric_set(mn_log_loss),
                            control = control_bayes(no_improve = 10, verbose = TRUE, save_pred = TRUE))

doParallel::stopImplicitCluster()


best_params <- as.data.table(select_best(xgboost_model))
out_of_fold_pred <- as.data.table(collect_predictions(xgboost_model))[.iter == as.integer(str_replace(best_params$.config, "Iter", ""))]

pred_cols <- names(out_of_fold_pred)[which(grepl(".pred", names(out_of_fold_pred), TRUE))]

mean_pred <- out_of_fold_pred[, lapply(.SD, mean), .SDcols = pred_cols]
highest_prob <- apply(out_of_fold_pred[, pred_cols, with = FALSE], 1, which.max)

s <- c(-1, -1, -2, 1, -2, 1, 5)
scores <- apply(out_of_fold_pred[, pred_cols, with = FALSE], 1, function(x){sum(x * s)})
scores <- as.data.table(scores)
names(scores) <- "Score"

ggplot(data = scores, aes(x = Score)) + 
	geom_density(alpha = 0.5, fill = "blue") +
	ylab("Density") +
	ggtitle("Score Density plot")

