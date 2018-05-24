# OBJECTS

# METHODS

setGeneric("getLocations", function(n, sd) standarGeneric("getLocations"))
setMethod("getLocations", signature("numeric", "numeric"),
	# /*
	#    Returns a data set of coordinates. 100 elements for a given mean
	# */
	#
	# /* n
	#    Numeric vector with the means of the groups you want to simulate.
	#    Standard deviation will set to 4.
	# */
	function(n, sd) {
		x <- c() # // x coordinates
		y <- c() # // y coordinates
		for (i in 1:length(n)) {
			x <- c(x, round(rnorm(n[i], sd[i], n=100)))
			y <- c(y, round(rnorm(n[i], sd[i], n=100)))
		}
		data <- cbind(x, y)
		rownames(data) <- apply(t(combn(LETTERS, m=4)), 1, paste, collapse="")[1:length(data[,1])]
		data # // returns a matrix with x and y coordinates
	})

setGeneric("findHDL", function(coords, radius) standarGeneric("findHDL"))
setMethod("findHDL", signature("matrix", "numeric"),
	# /* findHDL: find High-Density Location
	#    Finds the location in where a high points density is.
	# */
	function(coords, radius) {
		dist_matrix <- c()
		names_comb <- t(combn(rownames(coords), m=2))
		dist_matrix_names <- c()
		pb <- txtProgressBar(0, length(names_comb[,1]), style=3)
		for (i in 1:length(names_comb[,1])) {
			pointA <- coords[rownames(coords) %in% names_comb[i,1],]
			pointB <- coords[rownames(coords) %in% names_comb[i,2],]
			max.X <- max(pointA[1], pointB[1])
			min.X <- min(pointA[1], pointB[1])
			max.Y <- max(pointA[2], pointB[2])
			min.Y <- min(pointA[2], pointB[2])
			dist <- sqrt((max.X - min.X)^2 + (max.Y - min.Y)^2)
			if (dist > radius) {
				next
			}
			else {
				dist <- c(dist, names_comb[i,])
				dist_matrix_names <- c(dist_matrix_names, paste(names_comb[i,], collapse="x"))
				dist_matrix <- rbind(dist_matrix, dist)
			}
			setTxtProgressBar(pb, i)

		}
		rownames(dist_matrix) <- dist_matrix_names
		dist_matrix
	})

setGeneric("groupHDL", function(hdl) standarGeneric("groupHDL"))
setMethod("groupHDL", signature("matrix"),
	function(hdl) {
		groups <- list()
		group_counter <- 1
		pb <- txtProgressBar(0, length(hdl[,1]), style=3)
		groups[[paste("g", group_counter, sep="")]] <- hdl[1,2:length(hdl[1,])]
		for (i in 2:length(hdl[,1])) {
			#print(hdl[i, 2:length(hdl[1,])])
			#print(groups)
			keys <- c()
			for (j in 1:length(groups)) {
				if (sum(hdl[i,2:length(hdl[1,])] %in% groups[[paste("g", j, sep="")]]) > 0) {
					keys <- c(keys, names(groups)[j])
				}
			}
			if (length(keys) == 0) {
				group_counter <- group_counter + 1
				groups[[paste("g", group_counter, sep="")]] <- hdl[i,2:length(hdl[1,])]
			}
			else {
				for (key in keys) {
					groups[[key]] <- c(groups[[key]], hdl[i,2:length(hdl[1,])])
				}
			}
			setTxtProgressBar(pb, i)
		}
		for (name in names(groups)) {
			groups[[name]] <- unique(sort(groups[[name]]))
		}
		cat("\n")
		groups
	})

setGeneric("plotHDL", function(ghdl, coords, cutoff=0) standarGeneric("plotHDL"))
setMethod("plotHDL", signature("list", "matrix"),
	function(ghdl, coords, cutoff=0) {
		filt_ghdl <- list()
		for (name in names(ghdl)) {
			if (length(ghdl[[name]]) > cutoff) {
				filt_ghdl[[name]] <- ghdl[[name]]
			}
		}
		num_of_plots <- length(filt_ghdl)
		ncols <- 3
		if (num_of_plots < 3) {
			nrows <- 1
		}
		else {
			nrows <- as.integer(num_of_plots/ncols) + 1
		}
		par(mfrow=c(nrows, ncols))
		for (name in names(filt_ghdl)) {
			col <- ifelse(rownames(coords) %in% filt_ghdl[[name]], "red", "black")
			plot(coords, col=col)
		}
	})



