## This is the original construction heuristic which barely worked. A better implementation
## is in 2meta.R

# only minimize number of moves
# * Implement a construction heuristic
# * presented June 3, 2024.

#rm(list=ls())

track_name <- 'tracks/track_01'

pacman::p_load(
	magrittr, # pipe operators
	tibble,
	readr # write_csv
)


# load track in
track <- track_name %>%
	readLines() %>%
	strsplit('') %>%
	do.call(rbind, .)

# create a logical matrix that holds every position the car has traveled before,
# to prevent it from going back to the same place
track_traversed <- matrix(F, nrow=nrow(track), ncol=ncol(track))


# get positions
car_position <- which(track == 'S', arr.ind=T) %>% as.integer()
finish_positions <- which(track == 'F', arr.ind=T)

# mark starting position on track as traversed
track_traversed[car_position[1], car_position[2]] <- T

# table for moves
moves <- tibble(x=car_position[1], y=car_position[2])

momentum <- c(0, 0) # initial momentum
momentums <- tibble(x=momentum[1], y=momentum[2])

# to calculate scores (distance from finish) for each possible move
euclidean_distance <- \(x1, y1, x2, y2) {
	sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# to interpolate between the car position & a desired to step,
# in order to make sure it does not pass an obstacle
check_route <- \(from, to, block_character='O', resolution=10) {

	range_x <- seq(from[1], to[1], length.out=resolution)
	range_y <- seq(from[2], to[2], length.out=resolution)

	for (i in 1:length(range_x)) {

		if (track[round(range_x[i]), round(range_y[i])] == block_character) {
			return(F)
			break
		}
	}; rm(i)
	return(T)
}

# if car hits a barrier or barrier, go back some number of moves & only slow down
slowDownTimer <- 0
slowDownDefault <- 2

slowDownFreeMoves <- 0 # once it reaches slowDownFreeResetMax, reset slowDownDefault to 2
slowDownFreeResetMax <- 3

no_valid_moves <- F
failed <- F

while (track[car_position[1], car_position[2]] != 'F') {
	# create matrix of possible moves, based on current position & momentum
	next_position <- car_position + momentum

	# if the next position is out of bounds or hits an obstacle, set slow down counter
	if (
		nrow(moves) > (slowDownDefault+2) && (
			next_position[1] <= 1 || next_position[1] >= nrow(track) ||
			next_position[2] <= 1 || next_position[2] >= ncol(track) ||
			no_valid_moves
		)
	) {
		no_valid_moves <- F
		slowDownFreeMoves <- 0
		slowDownTimer <- slowDownDefault
		if (slowDownDefault < 10) {
			slowDownDefault %<>% `+`(1)
		}

		# undo traversal matrix
		for (i in 1:nrow(tail(moves, slowDownDefault))) {

			row <- moves[i,] %>% as.integer()

			track_traversed[row[1], row[2]] <- F
		}

		# go back 2 moves
		moves %<>% head(-slowDownDefault)
		momentums %<>% head(-slowDownDefault)




		# reset car position & momentum
		car_position <- moves %>% tail(1) %>% as.integer()
		momentum <- momentums %>% tail(1) %>% as.integer()

		next_position <- car_position + momentum
	} else {
		slowDownFreeMoves %<>% `+`(1)
		if (slowDownFreeMoves > slowDownFreeResetMax) {
			slowDownDefault <- 2
		}

	}

	if (
		next_position[1] <= 1 || next_position[1] >= nrow(track) ||
		next_position[2] <= 1 || next_position[2] >= ncol(track)
	) {
		next
		# problem: at edge, but cannot step back
	}


	track_subset <- track[
		(next_position[1]-1):(next_position[1]+1), # x
		(next_position[2]-1):(next_position[2]+1)  # y
	]

	# # # if all squares around it have been traversed, reset them
	# if (
	# 	sum(track_traversed[
	# 		(next_position[1]-1):(next_position[1]+1),
	# 		(next_position[2]-1):(next_position[2]+1)
	# 	]) == 9
	# ) {
	# 	# track_traversed[
	# 	# 	(next_position[1]-1):(next_position[1]+1),
	# 	# 	(next_position[2]-1):(next_position[2]+1)
	# 	# ] <- F
	# 	track_traversed <- matrix(F, nrow=nrow(track), ncol=ncol(track))
	# }




	track_subset_scores <- matrix(length(track), nrow=3, ncol=3)
	# i <- 3; j <- 1
	for (i in 1:3) {
		for (j in 1:3) {
			traversed <- track_traversed[i+next_position[1]-2, j+next_position[2]-2]
			if (track_subset[i, j] != 'O' && !traversed) {
				# before calculating its score, check if the path to it is blocked
				if (check_route(car_position, c(i+next_position[1]-2, j+next_position[2]-2))) {


					distances <- apply(finish_positions, 1, \(finish_block) {
						euclidean_distance(next_position[1] + i - 2, next_position[2] + j - 2, finish_block[1], finish_block[2])
					})

					track_subset_scores[i, j] <- min(distances)
					if (track_subset[i, j] == 'G') {
						track_subset_scores[i, j] %<>% `*`(2)

					}


					if (
						slowDownTimer > 0 && (
							momentum[1] < 0 && i < 3 ||
							momentum[1] > 0 && i > 1 ||
							momentum[2] < 0 && j < 3 ||
							momentum[2] > 0 && j > 1
						)

					) {

						track_subset_scores[i, j] %<>% `*`(4)


						slowDownTimer %<>% `-`(1)
					}

				}
			}
		}
	}; rm(i, j)
	no_valid_moves <- sum(track_subset_scores == length(track)) == 9
	if (!no_valid_moves) {
		# acquire indices of best move from track_subset_scores
		# if there are multiple, simply take the 1st one


		# best_moves <- which(track_subset_scores == min(track_subset_scores), arr.ind=T)
		#
		# best_moves_relative <- best_moves %>%
		# 	.[sample(1),] %>%
		# 	as.vector()

		best_move_relative <-
			which(track_subset_scores == min(track_subset_scores), arr.ind=T) %>%
				.[1,] %>%
				as.vector()
		best_move_absolute <- best_move_relative + next_position - 2

		# update momentum
		momentum %<>% `+`(best_move_relative - 2)
		momentums %<>% add_row(x=momentum[1], y=momentum[2])

		# check if move went through grass & penalize momentum
		if (!check_route(car_position, best_move_absolute, block_character='G')) {
			momentum %<>% sapply(\(value) {
				if (value > 0) {
					value %<>% `-`(1)
				} else if (value < 0) {
					value %<>% `+`(1)
				} else {
					value
				}
			})
		}

		# update car position
		car_position <- best_move_absolute

		# update track_traversed positions
		track_traversed[car_position[1], car_position[2]] <- T

		# record move
		moves %<>% add_row(x=best_move_absolute[1], y=best_move_absolute[2])
	}



	cat('-', nrow(moves))
	if (nrow(moves) > 1000) {
		#stop('\ncould not complete in 1000 moves')
		failed <- T
		break
	}
}


#cat('\nCompleted in ', nrow(moves), ' moves.\n')

write_csv(moves, 'moves.csv', append=F, col_names=F)






### Visualization
pacman::p_load(dplyr, ggplot2, tidyr)

# convert the matrix to a data frame
track_df <- as.data.frame(track, stringsAsFactors = FALSE)

# add row and column numbers for plotting
track_df <- track %>%
	as.tibble(stringsAsFactors=F) %>%
	mutate(., row = nrow(.):1) %>%
	pivot_longer(cols = -row, names_to = "column", values_to = "type") %>%
	mutate(column = as.numeric(gsub("V", "", column)))



# Adjust the row values to match the coordinate system of the plot
moves_flipped_x <- moves %>% rename(row=x, column=y)
moves_flipped_x$row <- nrow(track) - moves_flipped_x$row + 1

color_mapping <- c(
	"O" = "brown",
	"T" = "gray",
	"G" = "green",
	"S" = "yellow",
	"F" = "white"
)




ggplot() +

	geom_tile(data = track_df, aes(x = column, y = row, fill = type), color = "black") +
	scale_fill_manual(values = color_mapping) +

	geom_point(data = moves_flipped_x, aes(x = column, y = row), color = "purple", size = 3) +
	# lines connecting the moves
	geom_path(data = moves_flipped_x, aes(x = column, y = row), color = "blue", size = 1) +
	theme_minimal() +
	theme(
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		axis.title = element_blank(),
		legend.title = element_blank()
	) +
	coord_fixed() +
	ggtitle(ifelse(
		failed,
		paste(track_name, ' - Could not complete in 1000 moves'),
		paste(track_name, ' - Completed in', nrow(moves), 'moves')
	))








# simplified pseudocode for presentation

track <- matrix(file)
track_traversed <- matrix(F)
car_position <- which(track = 'S')
finish_positions <- which(track = 'F')

while (car_position != 'F') {
	next_position <- car_position + momentum
	track_subset <- track[next_position-1 : next_position+1]

	for (square in track_subset) {
		if (check_route(car_position, square, through='O' && !traversed)) {
			distance <- apply(finish_positions, euclidean_distance()) %>% min()
			if (square = 'G') distance *= random(1.5, 2)
			scores %<>% c(min(distance))
		}
	}

	best_move <- min(scores)

	momentum += track[best_move]

	if (check_route(car_position, best_move, through='G')) momentum -= 1

	car_position <- best_move_absolute

	track_traversed[car_position] <- T
}