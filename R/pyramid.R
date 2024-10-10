#' Class pyramid
#'
#' Data frame containing an age pyramid, with at least an \code{age} and a \code{value} column,
#' and eventually some other characteristics. If a \code{male} column is present, it must be a logical vector,
#' if a \code{group} column is present, it must be a vector of type character.
#'
#' @param x Data frame, containing at least an \code{age} and a \code{value} column
#'
#' @return Given data frame augmented of the "age_pyramid" class.
#'
#'
#' @docType class
#' @export
pyramid <- function(x){
  assertPyramid(x)
  class(x) <- c("pyramid", "data.frame")
  return(x)
}


#' Generic method for age_pyramid
#'
#' @param object Population.
#' @param time The age pyramid is computed at instant \code{time}. Must be a numeric greater than or equal to 0.
#' @param ages _(Optional)_ A numeric vector of distinct positive values composing age groups. Must be in increasing order.
#' @param ... Additional parameters
#'
#' @return An object of class \code{\link{pyramid}} containing the age pyramid of a population at instant \code{time}.
#'
#' @export
age_pyramid <- function(object, time = 0, ages = c(0:110, Inf), ...) {
  UseMethod("age_pyramid")
}


#' Age pyramid from a population at a given time.
#'
#' @description Reduce a population containing all individuals (with some characteristics) to an age-groups data frame (preserving characteristics). The function computes the number of individuals at \code{time} in each age group \code{[ages[i],ages[i+1][}, for \code{i} in \code{\{1,...,N-1\}}.
#'
#' @param object Object of \code{\link{population}} class representing a population.
#' @param time The age pyramid is computed at instant \code{time}. Must be a numeric greater than or equal to 0.
#' @param ages _(Optional)_ A numeric vector of distinct positive values composing age groups. Must be in increasing order.
#' @param ... Additional parameters
#'
#' @seealso \code{\link{age_pyramids.population}}
#'
#' @return An object of class \code{\link{pyramid}} containing the age pyramid of the given population at instant \code{time}.
#'
#' @examples
#' age_pyramid(population(EW_pop_14$sample), time = 0)
#'
#' age_pyramid(population(EW_popIMD_14$sample), time = 0, ages = seq(0, 120, by=2))
#'
#' @export
#'
age_pyramid.population <- function(object, time = 0, ages = c(0:110, Inf), ...)
{
  assertNumber(time, lower = 0, finite = TRUE)
  assertNumeric(ages, lower = 0, min.len = 2, unique = TRUE, sorted = TRUE)

  N <- length(ages)
  fact_ages <- factor(ages[1:(N-1)])
  levels(fact_ages) <- paste(ages[1:(N-1)], '-', ages[2:N])
  others <- setdiff(colnames(object), c('id', 'birth', 'death','out','entry'))
  # Select individuals present in the population at time t
  df_alive <- population_alive(object,t = time,ages[1],ages[N])

  # Compute their age at time t and select their characteristics
  df_temp <- df_alive %>%
    mutate(age = fact_ages[findInterval(time - .data$birth, ages[1:(N-1)])]) %>%
    select_at(c('age', others))

  d <- data.frame(df_temp %>% group_by_at(c('age', others)) %>%
                    summarise(value=n()))

  # rm age column and value column for expand.grid
  d_base <- d[-c(1,length(d))]

  out <- expand.grid(c(list('age'=levels(fact_ages)),lapply(as.list(d_base), unique)))
  names <- names(out)

  out <- dplyr::full_join(out,d, by = names)
  out["value"][is.na(out["value"])] <- 0

  return(pyramid(data.frame(out)))
}


#' Generic method for age_pyramids
#'
#' @inheritParams age_pyramid
#'
#' @export
age_pyramids <- function(object, time = 0, ages = c(0:110,Inf)) {
  UseMethod("age_pyramids")
}


#' Age pyramid from a population data frame at some given times.
#'
#' @description Vectorial version in time of the function \code{\link{age_pyramid.population}}. Not compatible with IBMs including swap events.
#' @inheritParams age_pyramid.population
#'
#' @details For convenience. This is a just a \code{lapply} call of \code{age_pyramid.population} on the vector \code{time}.
#'
#' @export
age_pyramids.population <- function(object, time = 0, ages = c(0:110,Inf)) {
    assertNumeric(time, lower = 0, unique = TRUE, finite = TRUE)
    assertNumeric(ages, lower = 0, min.len = 2, unique = TRUE, sorted = TRUE)

    pyrs <- do.call(rbind,
                lapply(time, function(t) {
                    cbind('time' = t, age_pyramid.population(object, time=t, ages))
                })
            )
    return(pyramid(pyrs))
}


#' Plot an age pyramid.
#'
#' @description Plot an age pyramid from age pyramid data frame with possibly several characteristics.
#'
#' @param x Object of class \code{\link{pyramid}}.
#'
#' _(Optional)_ For plotting an age pyramid composed of several subgroups, the population data frame must contain a column named \code{group_name}.
#' @param group_colors _(Optional)_ Named character vector.
#' @param group_legend _(Optional)_ Legend title name. By default set to \code{"Group"}.
#' @param age_breaks _(Optional)_ An ordered vector of indexes of vector \code{unique(pyr$age)} used for breaks for the axis of ages.
#' @param value_breaks _(Optional)_ Breaks for the axis of values.
#' @param ... Additional parameters
#'
#' @return Plot of the age pyramid.
#'
#' @seealso \code{\link{plot.population}}
#'
#' @examples
#' plot.pyramid(subset(pyramid(EW_pop_14$age_pyramid), as.numeric(age) <= 110))
#'
#'\donttest{
#' library(colorspace)
#' pyr_IMD <- subset(pyramid(EW_popIMD_14$age_pyramid), as.numeric(age) <= 110)
#' pyr_IMD$group_name <- with(pyr_IMD, ifelse(male, paste('Males - IMD', IMD),
#'                           paste('Females - IMD', IMD)))
#' colors <- c(sequential_hcl(n=5, palette = "Magenta"),
#'             sequential_hcl(n=5, palette = "Teal"))
#' names(colors) <- c(paste('Females - IMD', 1:5),
#'                    paste('Males - IMD', 1:5))
#' # note that you must have setequal(names(colors), pyr_IMD$group_name) is TRUE
#' plot.pyramid(pyr_IMD, colors)
#'
#' # age pyramids at different times
#' library(gganimate)
#' pyrs = age_pyramids(population(EW_popIMD_14$sample), time = 1:10)
#' plot.pyramid(pyrs) + transition_time(time) + labs(title = "Time: {frame_time}")
#'}
#' @export
#' @method plot pyramid
plot.pyramid <- function(x, group_colors = NULL, group_legend = 'Group',
                         age_breaks = NULL, value_breaks = NULL, ...) {
    assertPyramid(x)
    if ('group_name' %in% colnames(x) & !is.null(group_colors)) {
        assertCharacter(group_colors, any.missing = FALSE, unique = TRUE)
        assertNamed(group_colors, type = "unique")
        assertNames(x$group_name, subset.of = names(group_colors))
    }
    assertCharacter(group_legend)
    if (!is.null(age_breaks))
        assertInteger(age_breaks, lower = 0, upper = length(unique(x$age)),
                      sorted = TRUE, any.missing = FALSE)
    if (!is.null(value_breaks))
        assertNumeric(value_breaks, lower = 0, finite = TRUE,
                      sorted = TRUE, any.missing = FALSE)

    # Add 'group_name' by default 'Males' and 'Females' if column does not exists
    if (!'group_name' %in% colnames(x)) {
        if ('male' %in% colnames(x)) {
            if (is.null(group_colors)) {
                group_colors = c('Males' = '#67A9B6', 'Females' = '#CB6CA2')
                x$group_name <- ifelse(x$male, 'Males', 'Females')
            } else {
                assertCharacter(group_colors, len = 2, any.missing = FALSE)
                assertNamed(group_colors, type = "unique")
                x$group_name <- ifelse(x$male,
                                        names(group_colors)[1],
                                        names(group_colors)[2])
            }
        } else {
            # no group
            if (is.null(group_colors)) {
                group_colors = c('Individuals' = 'grey')
                x$group_name <- 'Individuals'
            } else {
                assertCharacter(group_colors, len = 1, any.missing = FALSE)
                assertNamed(group_colors, type = "unique")
                x$group_name <- names(group_colors)[1]
            }
        }
    }
    # Reduce by sum x to keep only 'age', 'value', 'group_name' and 'male', 'time' if exists
    df <- x %>%
            group_by_at(intersect(colnames(x), c('age', 'male', 'group_name', 'time'))) %>%
            summarise(value = sum(.data$value))
    # Males on the left...
    if ('male' %in% colnames(df)) {
        df$value <- ifelse(df$male, -df$value, df$value)
    }

    # by default if no age_breaks
    if (is.null(age_breaks)) {
        n_ages <- length(unique(df$age))
        delta <- ifelse(n_ages <= 20, 1, ceiling(length(unique(df$age)) / 200)*20)
        age_breaks <- sort(unique(df$age))[seq(1,n_ages,by=delta)]
    } else {
        age_breaks <- sort(unique(df$age))[age_breaks]
    }
    if ('male' %in% colnames(df)) {
        if (is.null(value_breaks)) {
            yval <- (df %>% group_by_at(intersect(colnames(df), c('age', 'male', 'time')))
                     %>% summarise(value = sum(.data$value)))$value
            # for symmetry
            ylim <- c(-max(abs(yval)), max(abs(yval)))
            value_breaks <- pretty(ylim, 7)
        } else {
            ylim <- c(-max(value_breaks), max(value_breaks))# for symetry
            value_breaks <- unique(c(-value_breaks, 0, value_breaks))
        }
    } else {
        if (is.null(value_breaks)) {
            yval <- (df %>% group_by_at(intersect(colnames(df), c('age', 'time')))
                     %>% summarise(value = sum(.data$value)))$value
            ylim <- c(0, max(yval))
            value_breaks <- pretty(ylim, 7)
        } else {
            ylim <- c(0, max(value_breaks))
        }
    }

    output <- ggplot(data = df,
                     aes(x = .data$age, y = .data$value,
                         fill = .data$group_name)) +
        xlab("Age") +
        ylab("Number of individuals") +
        scale_x_discrete(breaks = age_breaks, drop = FALSE) +
        scale_y_continuous(breaks = value_breaks, labels = abs(value_breaks), limits = ylim) +
        coord_flip() +
        scale_fill_manual(name = group_legend, values = group_colors, drop = FALSE) +
        geom_col(position = "stack", width = 1)
   return(output)
}


#' Plot the age pyramid of a population data frame (at a given time).
#'
#' @description Plot an age pyramid from age pyramid data frame with possibly several characteristics.
#' @param x Object of class \link{population}.
#' @param group_colors _(Optional)_ Named character vector.
#' @param group_legend _(Optional)_ Legend title name. By default set to \code{"Group"}.
#' @param age_breaks _(Optional)_ An ordered vector of indexes of vector \code{unique(pyr$age)} used for breaks for the axis of ages.
#' @param value_breaks _(Optional)_ Breaks for the axis of values.
#' @param ... Additional arguments
#' @return Plot of age pyramid.
#'
#' @seealso \code{\link{plot.pyramid}}, \code{\link{age_pyramid.population}}.
#'
#' @examples
#' plot(population(EW_pop_14$sample), time = 0)
#'
#' @export
#' @method plot population
plot.population <- function(x, group_colors = NULL, group_legend = 'Group',
                            age_breaks = NULL, value_breaks = NULL, ...) {
    return(plot.pyramid(age_pyramid(x, ...), group_colors,
                        group_legend, age_breaks, value_breaks))
}


#' Generic method for popsample
#'
#' @param age_pyramid Age pyramid.
#' @param size A non-negative integer giving the number of individuals in population.
#' @param age_max _(Optional)_ A non-negative numeric which replace (if exists) the \code{Inf} in \code{\link{age_pyramid.population}}.
#' @param time _(Optional)_ The age pyramid is computed at instant \code{time}. Must be a numeric greater than or equal to 0.
#'
#'
#' @return Object of \code{\link{population}} class representing a data frame of size \code{size} containing a population of individuals.
#'
#' @export
popsample <- function(age_pyramid, size, age_max = 120, time = 0) {
  UseMethod("popsample")
}


#' Sample a population from an age pyramid (at a given time).
#'
#' @param age_pyramid Object of \code{\link{pyramid}} class.
#' @param size A non-negative integer giving the number of individuals in population.
#' @param age_max _(Optional)_ A non-negative numeric which replace (if exists) the \code{Inf} in \code{\link{age_pyramid.population}}.
#' @param time _(Optional)_ The age pyramid is computed at instant \code{time}. Must be a numeric greater than or equal to 0.
#'
#' @return Object of \code{\link{population}} class representing a data frame of size \code{size} containing a population of individuals.
#'
#' @examples
#' pop_sample_1e4 <- popsample(pyramid(EW_pop_14$age_pyramid), size = 1e4)
#'
#' @export
#'
popsample.pyramid <- function(age_pyramid, size, age_max = 120, time = 0) {
    assertPyramid(age_pyramid)
    assertCount(size, positive = TRUE)
    assertNumber(age_max, lower = 0, finite = TRUE)
    assertNumber(time, lower = 0, finite = TRUE)

    idx <- sample(1:nrow(age_pyramid), size = size, replace = TRUE,
                 prob = age_pyramid$value)

    tmp <- sapply(strsplit(as.character(age_pyramid[idx,]$age), '-'), as.double)
    tmp[2, tmp[2,]==Inf] <- age_max
    delta = tmp[2,]-tmp[1,]

    result <- age_pyramid[idx,] %>%
        mutate(birth = time - (as.double(.data$age)-1
                               + delta*runif(length(.data$age))),
               death = NA) %>%
        select(.data$birth, .data$death, everything(), -.data$age, -.data$value)

    return(population(result))
}
