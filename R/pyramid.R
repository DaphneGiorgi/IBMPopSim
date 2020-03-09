#' Age pyramid from a population dataframe
#'
#' @param population Population dataframe where first columns are birth and death.
#' @param time  The age pyramid is computed at time `time`.
#' @param ages A vector of size N composed of age groups.
#' The function computes the number of individuals at `time` in each age group [ages[i],ages[i+1][, for i in 1...N-1.
#'
#' @return Age pyramid of a population at time `time`
#'
#' @examples
#' EW_age_pyramid <- age_pyramid(EW_pop_14$sample, 0)
#'
#' @export
#'
age_pyramid <- function(population, time, ages = c(0:110,Inf)) {
    N <- length(ages)
    # Select individuals present in the population at time t
    df_alive <- filter(population, (population[,"birth"] < time)
                       & (time - population[,"birth"]<=ages[N])
                       & (time - population[,"birth"]>=ages[1])
                       & (is.na(population[,"death"])
                          | (population[,"death"] > time)))
    # Compute their age at time t and select their characteristics
    df_temp <- df_alive %>%
        mutate(age = ages[findInterval(time - birth, ages[1:(N-1)])]) %>%
        select(c("age",3:ncol(df_alive)))
    # Frequency table (group_by_at to use string vector for column names)
    x <- sapply(colnames(df_temp)[-1], function(a) { as.name(a) })
    # Use of data.frame since complete does not seems to work with type
    df_pyr <- data.frame(df_temp %>%
                         group_by_at(colnames(df_temp),.drop = FALSE) %>%
                         summarise(value=n())) %>%
    complete("age"=ages[1:(N-1)],!!!x,fill=list(value=0)) # Complete with age groupes in which there are no individuals
    return(data.frame(df_pyr))
}

#' Plot a pyramid of ages
#'
#' @param pyramid Pyramid of ages of a population
#' @param age_cutoff Age max after which the individuals in the pyramid are grouped (as individuals older than age_cutoff)
#' @param age_break Age axis graduation break
#'
#' @return Plot of the pyramid of ages
#'
#' @examples
#' plot_pyramid(EW_pop_14$age_pyramid)
#'
#' @export
#'
plot_pyramid <- function(pyramid, age_cutoff=100, age_break=10) {
    pyr_cutoff <- pyramid %>% group_by(male) %>% filter(age >= age_cutoff) %>% summarise(value = sum(value))
    if (nrow(pyr_cutoff) > 0) {
        pyramid <- rbind(subset(pyramid, age < age_cutoff), data.frame(age=age_cutoff, pyr_cutoff))
        pyramid <- rbind(pyramid, data.frame(age=age_cutoff+1, male=c(TRUE,FALSE), value=0))
    }
    if (nrow(pyramid) == 0) {
        stop("pyramid empty")
    }
    xaxe <- seq(min(pyramid[,"age"]), max(pyramid[,"age"]), age_break)
    tmp <- pretty(pyramid$value)
    yaxe <- c(-rev(tmp[-1]), tmp)

    xlab <- xaxe
    xlab[length(xlab)] <- paste(xlab[length(xlab)], "+", sep="")
    plt <- ggplot(data = pyramid, aes(x = age, y = value)) +
        ggtitle("Age pyramid") +
        xlab("Age") +
        ylab("Number of individuals") +
        coord_flip() +
        scale_x_continuous(breaks = xaxe, labels = xlab) +
        scale_y_continuous(breaks = yaxe, labels = abs(yaxe)) +
        scale_fill_manual(values=c("#FD6C9E", "#1E7FCB")) +
        geom_bar(data = subset(pyramid, male == FALSE),
                 stat = "identity", position = "stack", mapping = aes(fill = male), width = 1) +
        geom_bar(data = subset(pyramid, male == TRUE),
                 stat = "identity", position = "stack", mapping = aes(fill = male, y = -value), width = 1) +
        geom_step(data = subset(pyramid, male == FALSE, select = c(age, value)),
                  mapping = aes(x = age - 1/2, y = value)) +
        geom_step(data = subset(pyramid, male == TRUE, select = c(age, value)),
                  mapping = aes(x = age - 1/2, y = -value))
    return(plt)
}

#' Plot the pyramid of ages of a population (at a given time)
#'
#' @param population Population
#' @param ... Other arguments passed to the method, same as age_pyramid and plot_pyramid arguments
#'
#' @return Plot of the pyramid of ages
#'
#' @examples
#' plot_population(population=EW_pop_out, time=100)
#'
#' @export
#'
plot_population <- function(population, age_cutoff=100, age_break=10, ...) {
    return(plot_pyramid(age_pyramid(population, ...), age_cutoff, age_break))
}

# t_start <- Sys.time()
#  test <- age_pyramid(population, 0)
# t_end <- Sys.time()

###########  A REPRENDRE

# plot_pyramid<- function(df_pyr,t,scale_pyr, xbreak =5,vectorcolorlegend_plot_val= c("1"="#1a9641","2"="#a6d96a","3"="#ffff33","4"="#ff7f00","5"="#d7191c","6"="black"),vectorcolorlines= c("1"="darkgreen","5"="darkred"),police_val="Helvetica",title_val=NULL) {

#     df_pyr[,"male"]<-as.factor(df_pyr[,"male"])
#     xaxe <- seq(min(df_pyr[,"age"]), max(df_pyr[,"age"]), xbreak)
#     xaxe2 <-seq(t-min(df_pyr[,"age"]), t-max(df_pyr[,"age"]), by = -xbreak)
#     tmp<-scale_pyr #pour fixer echelle max nombre individus
#     yaxe <- c(-rev(tmp[-1]), tmp)
#     xlab <- xaxe
#     xlab[length(xlab)] <- paste(xlab[length(xlab)], "+", sep="")
#     result <- ggplot() +
#         ggtitle(title_val)+
#         xlab("Age") +
#         ylab("Number of individuals (thousands)") +
#         coord_flip() +
#         theme_wsj(color="grey") +
#         scale_y_continuous(breaks = yaxe, labels = abs(yaxe)) +
#         scale_x_continuous(breaks = xaxe, labels = xlab, sec.axis=sec_axis(~(-.),breaks = rev(-xaxe), labels = t-xaxe, name="Year of birth")) +
#         # Couleur a l'interieur des pyramides
#         geom_bar(data = subset(df_pyr, male == TRUE), stat = "identity", position = "stack", mapping = aes(x = age, y = -Value,fill=male), width = 1) + #fill=!!char
#         geom_bar(data = subset(df_pyr, male == FALSE), stat = "identity", position = "stack", mapping = aes(x = age ,y = Value,fill=male), width = 1)# + #fill=!!char
#         # Pyramides des ages avec plusieurs sous populations, on peut tracer plusieurs pyramides grace a la commande group=Pop
#         #geom_step(data = subset(pyr, (male == TRUE )),mapping = aes(x = Age - 1/2, y = -Value,group=!!char,colour=Pop,alpha=Type))+
#        # geom_step(data = subset(pyr, (Type == FALSE)), mapping = aes(x = Age - 1/2, y = Value,group=!!char,colour=Pop,alpha=Type))# +
#         # Attention la ligne suivant ne marche que si la colonne Pop a ete cree avec "as.factor"
#         #scale_colour_manual(values=vectorcolorlines, name="Pop")   +
#         #scale_alpha_manual(values=c(0.5, 1)) +
#         #scale_fill_manual(values=vectorcolorlegend_plot_val,name="Gender") #+
#         #guides(alpha = guide_legend(reverse=TRUE),colour=FALSE)


#     return(result+  theme(
#         legend.box = "horizontal",axis.title=element_text(size=12,family=police_val),legend.title = element_text(size=12,family=police_val)))
# }


