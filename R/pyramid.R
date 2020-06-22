

#' Age pyramid from a population data frame at a given time.
#'
#' @description Reduce a population data frame containing all individuals (with some characteristics) to an age-groups data frame (preserving characteristics). The function computes the number of individuals at \code{time} in each age group \code{[ages[i],ages[i+1][}, for \code{i} in \code{\{1,...,N-1\}}.
#'
#' @param population Population data frame: characteristics in columns and individuals in rows. At least two columns \code{birth} (dates of birth) and \code{death} (dates of death) are required.
#' @param time The age pyramid is computed at instant \code{time}. Must be a numeric greater than or equal to 0.
#' @param ages _(Optional)_ A numeric vector of distinct positive values composing age groups. Must be in increasing order.
#'
#' @details See also \code{\link{age_pyramids}}.
#'
#' @return Age pyramid of a population at instant \code{time}. A data frame with columns \code{age} and \code{value}. Optional characteristics are preserved.
#'
#' @examples
#' age_pyramid(EW_pop_14$sample, time = 0)
#'
#' age_pyramid(EW_popIMD_14$sample, time = 0, ages = seq(0, 120, by=2))
#'
#' @export
#'
age_pyramid <- function(population, time = 0, ages = c(0:110, Inf))
{
    assertPopulation(population)
    assertNumber(time, lower = 0, finite = TRUE)
    assertNumeric(ages, lower = 0, min.len = 2, unique = TRUE, sorted = TRUE)

    N <- length(ages)
    fact_ages <- factor(ages[1:(N-1)])
    levels(fact_ages) <- paste(ages[1:(N-1)], '-', ages[2:N])
    others <- setdiff(colnames(population), c('id', 'birth', 'death','out','entry'))
    # Select individuals present in the population at time t
    df_alive <- population_alive(population,t = time,ages[1],ages[N])
    # Compute their age at time t and select their characteristics
    df_temp <- df_alive %>%
        mutate(age = fact_ages[findInterval(time - .data$birth, ages[1:(N-1)])]) %>%
        select_at(c('age', others))
    # Frequency table (group_by_at to use string vector for column names)
    df_pyr <- data.frame(df_temp %>% group_by_at(c('age', others)) %>%
                summarise(value=n())) %>%
                complete('age'=fact_ages[1:(N-1)],
                         !!!as.list(sapply(others, as.name)),
                         fill=list(value=0))
    return(data.frame(df_pyr))
}

#' Age pyramid from a population data frame at some given times.
#'
#' @description Vectorial version in time of the function \code{\link{age_pyramid}}. Not compatible with IBMs including swap events. 
#' @inheritParams age_pyramid
#' @param time The age pyramid is computed at instants \code{time}. Must be a numeric vector of greater than or equal to 0.
#'
#' @details For convenience. This is a just a \code{lapply} call of \code{age_pyramid} on the vector \code{time}.
#'
#' @export
#'
age_pyramids <- function(population, time = 0, ages = c(0:110,Inf)) {
    assertPopulation(population)
    assertNumeric(time, lower = 0, unique = TRUE, finite = TRUE)
    assertNumeric(ages, lower = 0, min.len = 2, unique = TRUE, sorted = TRUE)

    pyrs <- do.call(rbind,
                lapply(time, function(t) {
                    cbind('time' = t, age_pyramid(population, time=t, ages))
                })
            )
    return(pyrs)
}

#' Plot an age pyramid from age pyramid data frame.
#'
#' @description Plot an age pyramid from age pyramid data frame with possibly several characteristics. See also \code{\link{plot_population}}.
#'
#' @param pyramid Age pyramid of a population. Dataframe containing at least \code{age} and \code{value} columns.
#'
#' _(Optional)_ For plotting an age pyramid composed of several subgroups, the population data frame must contain a column named \code{group_name}.
#' @param group_colors _(Optional)_ Named character vector.
#' @param group_legend _(Optional)_ Legend title name. By default set to \code{"Group"}.
#' @param age_breaks _(Optional)_ An ordered vector of indexes of vector \code{unique(pyr$age)} used for breaks for the axis of ages.
#' @param value_breaks _(Optional)_ Breaks for the axis of values.
#'
#' @return Plot of the age pyramid.
#'
#' @examples
#' plot_pyramid(subset(EW_pop_14$age_pyramid, as.numeric(age) <= 110))
#'
#'\dontrun{
#' library(colorspace)
#' pyr_IMD <- subset(EW_popIMD_14$age_pyramid, as.numeric(age) <= 110)
#' pyr_IMD$group <- with(pyr_IMD, ifelse(male, paste('Males - IMD', IMD), paste('Females - IMD', IMD)))
#' colors <- c(sequential_hcl(n=5, palette = "Magenta"),
#'             sequential_hcl(n=5, palette = "Teal"))
#' names(colors) <- c(paste('Females - IMD', 1:5),
#'                    paste('Males - IMD', 1:5))
#' # note that you must have setequal(names(colors), pyr_IMD$group) is TRUE
#' plot_pyramid(pyr_IMD, colors)
#'}
#' @export
#'
plot_pyramid <- function(pyramid, group_colors, group_legend = 'Group',
                         age_breaks, value_breaks) {
    assertPyramid(pyramid)
    if ('group_name' %in% colnames(pyramid) & !missing(group_colors)) {
        assertCharacter(group_colors, any.missing = FALSE, unique = TRUE)
        assertNamed(group_colors, type = "unique")
        assertNames(pyramid$group_name, subset.of = names(group_colors))
    }
    assertCharacter(group_legend)
    if (!missing(age_breaks))
        assertInteger(age_breaks, lower = 0, upper = length(unique(pyramid$age)),
                      sorted = TRUE, any.missing = FALSE)
    if (!missing(value_breaks))
        assertNumeric(value_breaks, lower = 0, finite = TRUE,
                      sorted = TRUE, any.missing = FALSE)

    # Add 'group_name' by default 'Males' and 'Females' if column does not exists
    if (!'group_name' %in% colnames(pyramid)) {
        if ('male' %in% colnames(pyramid)) {
            if (missing(group_colors)) {
                group_colors = c('Males' = '#67A9B6', 'Females' = '#CB6CA2')
                pyramid$group_name <- ifelse(pyramid$male, 'Males', 'Females')
            } else {
                assertCharacter(group_colors, len = 2, any.missing = FALSE)
                assertNamed(group_colors, type = "unique")
                pyramid$group_name <- ifelse(pyramid$male,
                                        names(group_colors)[1],
                                        names(group_colors)[2])
            }
        } else {
            # no group
            if (missing(group_colors)) {
                group_colors = c('Individuals' = 'grey')
                pyramid$group_name <- 'Individuals'
            } else {
                assertCharacter(group_colors, len = 1, any.missing = FALSE)
                assertNamed(group_colors, type = "unique")
                pyramid$group_name <- names(group_colors)[1]
            }
        }
    }
    # Reduce by sum pyramid to keep only 'age', 'value', 'group_name' and 'male', 'time' if exists
    df <- pyramid %>%
            group_by_at(intersect(colnames(pyramid), c('age', 'male', 'group_name', 'time'))) %>%
            summarise(value = sum(.data$value))
    # Males on the left...
    if ('male' %in% colnames(df)) {
        df$value <- ifelse(df$male, -df$value, df$value)
    }

    # by default if no age_breaks
    if (missing(age_breaks)) {
        n_ages <- length(unique(df$age))
        delta <- ifelse(n_ages <= 20, 1, ceiling(length(unique(df$age)) / 200)*20)
        age_breaks <- sort(unique(df$age))[seq(1,n_ages,by=delta)]
    } else {
        age_breaks <- sort(unique(df$age))[age_breaks]
    }
    if ('male' %in% colnames(df)) {
        if (missing(value_breaks)) {
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
        if (missing(value_breaks)) {
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
#' @description Plot an age pyramid from age pyramid data frame with possibly several characteristics. See also \code{\link{plot_pyramid}} and \code{\link{age_pyramid}}.
#' @param population Population data frame, with at least \code{birth} and \code{death} column.
#' @inheritParams plot_pyramid
#' @param ... Other arguments passed to \code{\link{age_pyramid}} (including \code{time}).
#'
#' @return Plot of age pyramid.
#'
#' @examples
#' plot_population(EW_pop_14$sample, time = 0)
#'
#' @export
#'
plot_population <- function(population, group_colors, group_legend = 'Group',
                            age_breaks, value_breaks, ...) {
    return(plot_pyramid(age_pyramid(population, ...), group_colors,
                        group_legend, age_breaks, value_breaks))
}


#' Sample a population from an age pyramid (at a given time).
#'
#' @param age_pyramid A data frame with columns \code{age}, \code{value} and others optional characteristics.
#' @param size A non-negative integer giving the number of individuals in population.
#' @param age_max _(Optional)_ A non-negative numeric which replace (if exists) the \code{Inf} in \code{\link{age_pyramid}}.
#' @param time _(Optional)_ The age pyramid is computed at instant \code{time}. Must be a numeric greater than or equal to 0.
#'
#' @return A data frame of size \code{size} containing a population of individuals.
#'
#' @examples
#' pop_sample_1e4 <- popsample(EW_pop_14$age_pyramid, size = 1e4)
#'
#' @export
#'
popsample <- function(age_pyramid, size, age_max = 120, time = 0) {
    assertPyramid(age_pyramid)
    assertCount(size, positive = TRUE)
    assertNumber(age_max, lower = 0, finite = TRUE)
    assertNumber(time, lower = 0, finite = TRUE)

    idx <- sample(1:nrow(age_pyramid), size = size, replace = TRUE,
                 prob = age_pyramid$value)

    # est-ce vraiment necessaire ? on pourrait mettre delta =
    tmp <- sapply(strsplit(as.character(age_pyramid[idx,]$age), '-'), as.double)
    tmp[2, tmp[2,]==Inf] <- age_max
    delta = tmp[2,]-tmp[1,]

    result <- age_pyramid[idx,] %>%
        mutate(birth = time - (as.double(.data$age)-1
                               + delta*runif(length(.data$age))),
               death = NA) %>%
        select(.data$birth, .data$death, everything(), -.data$age, -.data$value)
    return(result)
}
