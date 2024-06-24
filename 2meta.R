# Implement a metaheuristic-based approach
# • present June 24, 2024

track_name <- 'tracks/track_02.t'

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

# load track in as a character matrix
track <- track_name %>%
	readLines() %>%
	strsplit('') %>%
	do.call(rbind, .)

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
# from any finish position.
# When mapping over a list of each finish position & combine their resulting matrices
# using logical OR.
visible_fin <- map_matrix(track, \(x_ind, y_ind) {
	# same thing as before, except for a list of finish positions
	map_lgl(nrow(finish_positions), \(finish_position_index) {
		!check_route_hits(finish_positions[finish_position_index,], c(x_ind, y_ind))
	})
})




## Create initial line segments


# do the same,
# from the cars position
visible_car <- map_matrix(track, \(x_ind, y_ind) {
	# acquire a logical scalar of each cell's visibility from the car position
	!check_route_hits(car_position, c(x_ind, y_ind), hits_what='O')
})
# plot_track(with=visible_car)
# plot_track(with=visible_fin)





if (!any(overlap <- visible_car & visible_fin)) {
	#plot_track(with=overlap)
	
	# when there is no overlap between all visible cells
	# in both from the car's current position & from the finish positions
	
	# update visible history for candidate positions
	visible_history %<>% `|`(visible_car) %>% `|`(visible_fin)
	# plot_track(with=visible_history)

	
	# using the visibility matrices, calculate the best candidate position within that
	# contiguous region
	candidate_car_positions <- map_matrix(track, \(x_ind, y_ind) {
		# check positions currently visible to the car
		visible_car[x_ind, y_ind] &&
			# but have a desired neighbor that has not been seen before
			has_neighbor(x_ind, y_ind, 'T', visible_car | visible_history)
	})
	#plot_track(with=candidate_car_positions)
	candidate_finish_positions <- map_matrix(track, \(x_ind, y_ind) {
		visible_fin[x_ind, y_ind] &&
			has_neighbor(x_ind, y_ind, 'T', visible_fin | visible_history)
	})
	#plot_track(with=candidate_finish_positions)
	plot_track(with_region=candidate_car_positions | candidate_finish_positions)
	# find the best pair of candidates
	
	

	
	candid_car <- which(candidate_car_positions, arr.ind=T)
	candid_fin <- which(candidate_finish_positions, arr.ind=T)
	candidate_distances <- expand.grid(
		1:nrow(candid_car),
		1:nrow(candid_fin)
	) %>%
		rename(car_row = Var1, fin_row = Var2) %>%
		mutate(distance = map2_dbl(car_row, fin_row, \(car_row, fin_row) {
			euclidean_distance(candid_car[car_row,], candid_fin[fin_row,]) +
				ifelse(
					has_neighbor(candid_car[car_row,][1], candid_car[car_row,][2], 'G'),
					runif(1, .5, 1),
					0
				) +
				ifelse(
					has_neighbor(candid_fin[fin_row,][1], candid_fin[fin_row,][2], 'G'),
					runif(1, .5, 1),
					0
				
				)
		}))
	
	# update car position & finish position
	best_row <- slice_min(candidate_distances, distance, n =1)
	car_position <- best_row$car_row %>% {c(candid_car[., ][[1]], candid_car[.,][[2]])}
	path_car %<>% add_row(x=car_position[1], y=car_position[2])
	path_fin %<>% add_row(
		x = candid_fin[best_row$fin_row,][[1]],
		y = candid_fin[best_row$fin_row,][[2]]
	)
	
	plot_track(with_path=path_car)
	
	
} else {
	#plot_region(overlap)
	# repeat! (TODO: probably wrap in while loop)
	
	
}
















