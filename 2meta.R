# Implement a metaheuristic-based approach
# • present June 24, 2024

rm(list=ls())
track_name <- 'tracks/track_10.t'


start.time <- Sys.time()

source('custom_functions.R')
pacman::p_load(
	magrittr, # pipe operators
	tibble,
	ggplot2,
	readr, # write_csv
	purrr, # maps
	dplyr, # mutate
	tidyr # pivot_longer
)

track_num <- track_name %>%
	strsplit('/') %>%
	.[[1]] %>%
	gsub('.t', '', .) %>%
	.[2]
output_folder <- track_num %>%
	paste0('output/', ., '/')


# load track in as a character matrix
track <- track_name %>%
	readLines() %>%
	strsplit('') %>%
	do.call(rbind, .)


step_counter <- 1


# create a logical matrix that holds every position that has been viewed,
# to prevent it from considering those positions again
visible_history <- matrix(F, nrow=nrow(track), ncol=ncol(track))

# get an integer vector of length 2 indicating the car's coordinates
car_position <- which(track == 'S', arr.ind=T) %>% as.integer()
# get a matrix of n x 2 indicating the finish positions,
# where n is the number of finish positions
finish_positions <- which(track == 'F', arr.ind=T)

path_car <- tibble(x=car_position[1], y=car_position[2])
path_fin <- tibble(x=integer(), y=integer())

# Create a matrix with the same dimensions as track indicating every cell currently visible
# from the cars position
visible_car <- map_matrix(track, \(x_ind, y_ind) {
	# acquire a logical scalar of each cell's visibility from the car position
	!check_route_hits(car_position, c(x_ind, y_ind), hits_what='O')
})
save_plot(with_region=visible_car, tag='car visibility')
step_counter %<>% `+`(1)
# from any finish position.
# When mapping over a list of each finish position & combine their resulting matrices
# using logical OR.
visible_fin <- map_matrix(track, \(x_ind, y_ind) {
	# same thing as before, except for a list of finish positions
	map_lgl(nrow(finish_positions), \(finish_position_index) {
		!check_route_hits(finish_positions[finish_position_index,], c(x_ind, y_ind))
	})
})
save_plot(with_region=visible_fin, tag='finish visibility')
step_counter %<>% `+`(1)




## Create initial line segments


# do the same,

# plot_track(with_region=visible_fin)


while (!any(overlap <- visible_car & visible_fin)) {

	# when there is no overlap between all visible cells
	# in both from the car's current position & from the finish positions
	
	# update visible history for candidate positions
	visible_history %<>% `|`(visible_car) %>% `|`(visible_fin)
	# plot_track(with_region=visible_history)

	
	# using the visibility matrices, calculate the best candidate position within that
	# contiguous region
	candidate_car_positions <- map_matrix(track, \(x_ind, y_ind) {
		# check positions currently visible to the car
		visible_car[x_ind, y_ind] &&
			# but have a desired neighbor that has not been seen before
			has_neighbor(x_ind, y_ind, 'T', visible_car | visible_history)
	})
	candidate_finish_positions <- map_matrix(track, \(x_ind, y_ind) {
		visible_fin[x_ind, y_ind] &&
			has_neighbor(x_ind, y_ind, 'T', visible_fin | visible_history)
	})
	
	save_plot(with_region=candidate_car_positions, with_region2=candidate_finish_positions, tag='candidates')
	step_counter %<>% `+`(1)
	# find the best pair of candidates
	
	

	# get coordinates of all candidate positions
	candid_car <- which(candidate_car_positions, arr.ind=T)
	candid_fin <- which(candidate_finish_positions, arr.ind=T)
	
	# go through every combination of them to calculate the distances
	candidate_distances <- expand.grid(
		1:nrow(candid_car),
		1:nrow(candid_fin)
	) %>%
		rename(car_row = Var1, fin_row = Var2) %>%
		mutate(distance = map2_dbl(car_row, fin_row, \(car_row, fin_row) {
			euclidean_distance(candid_car[car_row,], candid_fin[fin_row,]) +
				# penalize if the candidate is near grass
				ifelse(
					has_neighbor(candid_car[car_row,][1], candid_car[car_row,][2], 'G'),
					runif(1, 2, 4),
					0
				) +
				ifelse(
					has_neighbor(candid_fin[fin_row,][1], candid_fin[fin_row,][2], 'G'),
					runif(1, 2, 4),
					0
				
				)
		}))
	
	# update car position & finish position
	best_row <- slice_min(candidate_distances, distance, n=1)
	car_position <- best_row$car_row %>% {c(candid_car[., ][[1]], candid_car[., ][[2]])}
	fin_position <- best_row$fin_row %>% {c(candid_fin[., ][[1]], candid_fin[., ][[2]])}
	path_car %<>% add_row(x=car_position[1], y=car_position[2])
	path_fin %<>% add_row(x = fin_position[1], y = fin_position[2])
	
	#plot_track(with_region=overlap)
	save_plot(
		with_region = candidate_car_positions | candidate_finish_positions,
		with_region2 = overlap,
		with_path = path_car,
		with_path2 = path_fin,
		tag = 'everything'
	)
	step_counter %<>% `+`(1)
	
	
	# recalculate whats visible
	visible_car <- map_matrix(track, \(x_ind, y_ind) {
		# acquire a logical scalar of each cell's visibility from the car position
		!check_route_hits(car_position, c(x_ind, y_ind), hits_what='O')
	})
	
	visible_fin <- map_matrix(track, \(x_ind, y_ind) {
		!check_route_hits(fin_position, c(x_ind, y_ind), hits_what='O')
	})
	

	
}

save_plot(
	with_region = overlap,
	tag = 'overlap'
)
step_counter %<>% `+`(1)

# if there is overlap check if the car can be immeidately
# connected to the finish, meaning that the finish position
# is visible from the car's current position

# for simplicity however, a single finish position value needs to be generated
if (!exists('fin_position')) {
	fin_distances <- map_matrix(visible_fin, \(x_ind, y_ind) {
		if (visible_fin[x_ind, y_ind]) {
			euclidean_distance(car_position, c(x_ind, y_ind)) +
				ifelse(
					has_neighbor(x_ind, y_ind, 'G'),
					runif(1, 2, 5),
					0
				)
		} else {
			length(track)
		}
	}, type_function=as.double)
	
	fin_position <- which(fin_distances == min(fin_distances), arr.ind=T)[1,] %>% as.integer()
	path_fin %<>% add_row(x=fin_position[1], y=fin_position[2])
	save_plot(with_path2=tibble(x=fin_position[1], y=fin_position[2]), tag='extend finish position')
	step_counter %<>% `+`(1)
}

if (!visible_car[fin_position[1], fin_position[2]]) {
	
	# if not, we simply find a nice midpoint to connect them
	
	# overlap_distances <- map_matrix(overlap, \(x_ind, y_ind) {
	# 	if (
	# 		overlap[x_ind, y_ind] &&
	# 		!check_route_hits(car_position, c(x_ind, y_ind)) &&
	# 		!check_route_hits(fin_position, c(x_ind, y_ind))
	# 	) {
	# 		#print(paste('\nsuccess (', x_ind, y_ind, ')--', distance_val))
	# 		euclidean_distance(car_position, c(x_ind, y_ind)) +
	# 			euclidean_distance(fin_position, c(x_ind, y_ind)) +
	# 			# penalize if this point is in the grass
	# 			ifelse(
	# 				has_neighbor(x_ind, y_ind, 'G'),
	# 				runif(1, 1, 2),
	# 				0
	# 			)
	#
	# 	} else {
	# 		nrow(track) * ncol(track)
	# 	}
	# }, type_function=as.double)
	#
	# connector <- which(overlap_distances == min(overlap_distances), arr.ind=T)[1,] %>%
	# 	as.integer()
	#
	
	connector <- which(overlap, arr.ind=T)[1, ] %>% as.integer()


	path_car %<>% add_row(x=connector[1], y=connector[2])
	save_plot(with_path=path_car, with_path2=path_fin, tag='connected')
	step_counter %<>% `+`(1)
}

path_car %<>% rbind(path_fin %>% map_df(rev))


save_plot(with_path=path_car, tag='finished')
step_counter %<>% `+`(1)




### actually do route



# table for moves
car_position <- path_car[1,] %>% as.integer()
moves <- tibble(x=car_position[1], y=car_position[2])
track_traversed <- matrix(F, nrow=nrow(track), ncol=ncol(track))
track_traversed[moves[1,][1] %>% as.integer(), moves[1,][2] %>% as.integer()] <- T
momentum <- c(0, 0) # initial momentum
momentums <- tibble(x=momentum[1], y=momentum[2])

path_index <- 2
target <- path_car[path_index,] %>% as.integer()



while (track[car_position[1], car_position[2]] != 'F') {
	next_position <- car_position + momentum
	
	track_subset <- get_subset(track, next_position[1], next_position[2])
	track_subset_scores <- matrix(length(track), nrow=3, ncol=3)
	# i <- 1; j <- 3
	for (i in 1:3) {
		for (j in 1:3) {
			traversed <- track_traversed[i+next_position[1]-2, j+next_position[2]-2]
			if (track_subset[i, j] != 'O' && !traversed) {
				# before calculating its score, check if the path to it is blocked
				if (!check_route_hits(car_position, c(i+next_position[1]-2, j+next_position[2]-2), 'O')) {
					
					if (path_index > nrow(path_car)) {
						distances <- apply(finish_positions, 1, \(finish_block) {
							euclidean_distance(c(next_position[1] + i - 2, next_position[2] + j - 2), finish_block)
						})
						
						track_subset_scores[i, j] <- min(distances)
					} else {
						track_subset_scores[i, j] <- euclidean_distance(c(i+next_position[1]-2, j+next_position[2]-2), target)
					}
					
					if (momentum[1] < 0 && i < 3) {
						track_subset_scores[i, j] %<>% `+`(abs(momentum[1]*runif(1, 2, 4))/(next_position[1])) + (3-i)
					}
					if (momentum[1] > 0 && i > 1) {
						track_subset_scores[i, j] %<>% `+`((momentum[1]*runif(1, 2, 4))/(nrow(track) - next_position[1])) + i
					}
					if (momentum[2] < 0 && j < 3) {
						track_subset_scores[i, j] %<>% `+`(abs(momentum[2]*runif(1, 2, 4))/next_position[2]) + (3-j)
					}
					if (momentum[2] > 0 && j > 1) {
						track_subset_scores[i, j] %<>% `+`((momentum[2]*runif(1, 2, 4))/(ncol(track)/next_position[2])) + j
					}
					
					
					if (track_subset[i, j] == 'G') {
						track_subset_scores[i, j] %<>% `*`(1.2)
						
					}
		
					
				}
			}
		}
	}; rm(i, j)
	
	best_move_relative <-
		which(track_subset_scores == min(track_subset_scores), arr.ind=T) %>%
			.[1,] %>%
			as.vector()
	best_move_absolute <- best_move_relative + next_position - 2
	
	# update momentum
	momentum %<>% `+`(best_move_relative - 2)
	momentums %<>% add_row(x=momentum[1], y=momentum[2])
	
	# check if move went through grass & penalize momentum
	if (check_route_hits(car_position, best_move_absolute, hits_what ='G')) {
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
	
	# # penalize anyway because it keep crashing
	# momentum %<>% sapply(\(value) {
	# 	if (value > 6) {
	# 		value %<>% `-`(1)
	# 	} else if (value < 6) {
	# 		value %<>% `+`(1)
	# 	} else {
	# 		value
	# 	}
	# })
	#
	# update car position
	car_position <- best_move_absolute
	
	# update track_traversed positions
	track_traversed[car_position[1], car_position[2]] <- T
	
	# record move
	moves %<>% add_row(x=best_move_absolute[1], y=best_move_absolute[2])
	
	save_plot(with_path=moves, with_path2=path_car, tag=paste('moving, goal:', path_index))
	step_counter %<>% `+`(1)
	
	if (path_index <= nrow(path_car)) { # check if already in finish line mode
		# check if any finish lines are in sight
		visible_fin <- map_matrix(track, \(x_ind, y_ind) {
			# same thing as before, except for a list of finish positions
			map_lgl(nrow(finish_positions), \(finish_position_index) {
				!check_route_hits(finish_positions[finish_position_index,], c(x_ind, y_ind))
			}) #&& !check_route_hits(car_position, c(x_ind, y_ind), hits_what='O')
		})
		if (visible_fin[car_position[1], car_position[2]]) {
			# if so, update target
			path_index <- nrow(path_car) + 1
			
		}
	}
	if (path_index < nrow(path_car)) {
		 # look for the next target in guide path
		visible_car <- map_matrix(track, \(x_ind, y_ind) {
			# acquire a logical scalar of each cell's visibility from the car position
			!check_route_hits(car_position, c(x_ind, y_ind), hits_what='O')
		})
		
		next_path_goal <- path_car[path_index+1,] %>% as.integer()
		
		if (visible_car[next_path_goal[1], next_path_goal[2]]) {
			target <- next_path_goal
			path_index %<>% `+`(1)
		}
	}
}
cat(Sys.time() - start.time)
sink("times.txt", append = T)
'--------------'
format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
track_num
paste('moves: ', nrow(moves))
Sys.time() - start.time
'--------------'
sink()