# day 19

library(tidyverse)

input <- readLines("inputs/day19.txt")
input <- readLines("inputs/day19_sample.txt")

input <- str_remove_all(input, "[a-zA-Z]|:|\\.") %>%
    trimws() %>%
    str_split("\\s+")


blueprint <- input[[i]]
blueprint <- as.integer(blueprint)

ore_robot_cost <- blueprint[2]

clay_robot_cost <- blueprint[3]

obsidian_robot_cost_ore <- blueprint[4]
obsidian_robot_cost_clay <- blueprint[5]

geode_robot_cost_ore <- blueprint[6]
geode_robot_cost_obsidian <- blueprint[7]

max_ore_cost <- max(blueprint[c(2, 4, 6)])

geode_list <- list(0)
full_list <- list()

# ===================================
# new method ========================
# ===================================

f_add_minerals <- function(minerals, robots) {
    minerals["ore"] <- minerals["ore"] + robots["ore"]
    minerals["clay"] <- minerals["clay"] + robots["clay"]
    minerals["obsidian"] <- minerals["obsidian"] + robots["obsidian"]
    minerals["geode"] <- minerals["geode"] + robots["geode"]
    return(minerals)
}

f_decision <- function(minutes, minerals, robots) {

    for (m in minutes:24) {

        options <- c()
        if (minerals["ore"] >= geode_robot_cost_ore &&
            minerals["obsidian"] >= geode_robot_cost_obsidian
            ) {
                options[length(options) + 1] <- 5
        }
        if (minerals["ore"] >= obsidian_robot_cost_ore &&
            minerals["clay"] >= obsidian_robot_cost_clay &&
            #m < 22
            m < 22) {
                options[length(options) + 1] <- 4
        }
        if (minerals["ore"] >= clay_robot_cost &&
            #m < 20
            m < 17) {
                options[length(options) + 1] <- 3
        }
        if (minerals["ore"] >= ore_robot_cost &&
            #m < 22 
            m < 11 &&
            robots["ore"] < max_ore_cost) {
                options[length(options) + 1] <- 2
        }

        minerals <- f_add_minerals(minerals, robots)


        if (m == 24) {
            local_geodes <- append(geode_list, minerals["geode"])
            assign("geode_list", local_geodes, envir = .GlobalEnv)

            local_full <- append(full_list, list(c(minerals, robots)))
            assign("full_list", local_full, envir = .GlobalEnv)

            return()
        }

        # pruning
        if (
            m > 16 && robots["clay"] < 1 ||
            m > max(ore_robot_cost, clay_robot_cost) + 1 && sum(robots) == 1 ||
            # possible geodes < max geodes
            minerals["geode"] + (24 - m) * robots["geode"] +
                (24 - m) * (24 - m + 1) / 2 <
                max(unlist(geode_list)) ||
            # possible one geode robot
            minerals["obsidian"] + (22 - m) * robots["obsidian"] +
                (22 - m) * (22 - m + 1) / 2 < geode_robot_cost_obsidian &&
                robots["geode"] == 0 ||
            # possible one obsidian robot
            minerals["clay"] + (20 - m) * robots["clay"] +
                (20 - m) * (20 - m + 1) / 2 < obsidian_robot_cost_clay &&
                robots["obsidian"] == 0

        ) return()


        for (i in seq_along(options)) {
            if (options[i] == 5) {
                minerals_local <- minerals
                robots_local <- robots

                robots_local["geode"] <- robots_local["geode"] + 1
                minerals_local["ore"] <-
                    minerals_local["ore"] - geode_robot_cost_ore
                minerals_local["obsidian"] <-
                    minerals_local["obsidian"] - geode_robot_cost_obsidian

                f_decision(m + 1, minerals_local, robots_local)
            }
            else if (options[i] == 4) {
                minerals_local <- minerals
                robots_local <- robots

                robots_local["obsidian"] <- robots_local["obsidian"] + 1
                minerals_local["ore"] <-
                    minerals_local["ore"] - obsidian_robot_cost_ore
                minerals_local["clay"] <-
                    minerals_local["clay"] - obsidian_robot_cost_clay

                f_decision(m + 1, minerals_local, robots_local)
            }
            else if (options[i] == 3) {
                minerals_local <- minerals
                robots_local <- robots

                robots_local["clay"] <- robots_local["clay"] + 1
                minerals_local["ore"] <- minerals_local["ore"] - clay_robot_cost

                f_decision(m + 1, minerals_local, robots_local)
            }
            else if (options[i] == 2) {
                minerals_local <- minerals
                robots_local <- robots

                robots_local["ore"] <- robots_local["ore"] + 1
                minerals_local["ore"] <- minerals_local["ore"] - ore_robot_cost

                f_decision(m + 1, minerals_local, robots_local)
            }

        }


    }

}

v <- c()
# for (i in 1) {
for (i in seq_along(input)) {

    blueprint <- input[[i]]
    blueprint <- as.integer(blueprint)

    ore_robot_cost <- blueprint[2]

    clay_robot_cost <- blueprint[3]

    obsidian_robot_cost_ore <- blueprint[4]
    obsidian_robot_cost_clay <- blueprint[5]

    geode_robot_cost_ore <- blueprint[6]
    geode_robot_cost_obsidian <- blueprint[7]

    max_ore_cost <- max(blueprint[c(2, 4, 6)])


    minerals_global <- c(ore = 0, clay = 0, obsidian = 0, geode = 0)
    minerals_global
    robots_global <- c(ore = 1, clay = 0, obsidian = 0, geode = 0)
    robots_global

    geode_list <- list(0)
    full_list <- list()

    if (ore_robot_cost + clay_robot_cost >= 8 &&
        obsidian_robot_cost_clay >= 14 &&
        geode_robot_cost_obsidian >= 12) {
            v[i] <- 0

    } else {
        f_decision(1, minerals_global, robots_global)
        v[i] <- max(unlist(geode_list))
    }
}

sum(v * seq_along(v))


 # p2 ===================================================

input <- readLines("inputs/day19.txt")
input <- readLines("inputs/day19_sample.txt")

input <- str_remove_all(input, "[a-zA-Z]|:|\\.") %>%
    trimws() %>%
    str_split("\\s+")

input <- head(input, 3)



f_decision <- function(minutes, minerals, robots) {

    for (m in minutes:32) {

        options <- c()
        if (minerals["ore"] >= geode_robot_cost_ore &&
            minerals["obsidian"] >= geode_robot_cost_obsidian
            ) {
                options[length(options) + 1] <- 5
        }
        if (minerals["ore"] >= obsidian_robot_cost_ore &&
            minerals["clay"] >= obsidian_robot_cost_clay &&
            m < 30 &&
            robots["obsidian"] < geode_robot_cost_obsidian) {
                options[length(options) + 1] <- 4
        }
        if (minerals["ore"] >= clay_robot_cost &&
            m < 18 &&
            robots["clay"] < obsidian_robot_cost_clay) {
                options[length(options) + 1] <- 3
        }
        if (minerals["ore"] >= ore_robot_cost &&
            m < 11 &&
            robots["ore"] < max_ore_cost) {
                options[length(options) + 1] <- 2
        }


        minerals <- f_add_minerals(minerals, robots)


        if (m == 32) {
            local_geodes <- append(geode_list, minerals["geode"])
            assign("geode_list", local_geodes, envir = .GlobalEnv)

            local_full <- append(full_list, list(c(minerals, robots)))
            assign("full_list", local_full, envir = .GlobalEnv)

            return()
        }

        # pruning
        if (
            # m > ore_robot_cost + 1 && robots["ore"] < 2 ||
            m > 18 && robots["clay"] < 1 ||
            m > max(ore_robot_cost, clay_robot_cost) + 1 && sum(robots) == 1 ||
            m > max_ore_cost + 2 && sum(robots) < 2 ||
            m > max_ore_cost * 2 + 2 && sum(robots) < 3 ||
            m > max_ore_cost * 3 + 2 && sum(robots) < 4 ||
            m > ore_robot_cost + 1 && robots["ore"] == 0 ||
            m > 10 && robots["clay"] == 0 ||
            m > 14 && robots["clay"] < 2 ||
            m > 17 && robots["clay"] < 3 ||
            # possible geodes < max geodes
            minerals["geode"] + (32 - m) * robots["geode"] +
                (32 - m) * (32 - m + 1) / 2 <
                max(unlist(geode_list)) ||
            # possible one geode robot
            minerals["obsidian"] + (30 - m) * robots["obsidian"] +
                (30 - m) * (30 - m + 1) / 2 < geode_robot_cost_obsidian &&
                robots["geode"] == 0 ||
            # possible one obsidian robot
            minerals["clay"] + (28 - m) * robots["clay"] +
                (28 - m) * (28 - m + 1) / 2 < obsidian_robot_cost_clay &&
                robots["obsidian"] == 0

        ) return()


        for (i in seq_along(options)) {
            if (options[i] == 5) {
                minerals_local <- minerals
                robots_local <- robots

                robots_local["geode"] <- robots_local["geode"] + 1
                minerals_local["ore"] <-
                    minerals_local["ore"] - geode_robot_cost_ore
                minerals_local["obsidian"] <-
                    minerals_local["obsidian"] - geode_robot_cost_obsidian

                f_decision(m + 1, minerals_local, robots_local)
            }
            else if (options[i] == 4) {
                minerals_local <- minerals
                robots_local <- robots

                robots_local["obsidian"] <- robots_local["obsidian"] + 1
                minerals_local["ore"] <-
                    minerals_local["ore"] - obsidian_robot_cost_ore
                minerals_local["clay"] <-
                    minerals_local["clay"] - obsidian_robot_cost_clay

                f_decision(m + 1, minerals_local, robots_local)
            }
            else if (options[i] == 3) {
                minerals_local <- minerals
                robots_local <- robots

                robots_local["clay"] <- robots_local["clay"] + 1
                minerals_local["ore"] <- minerals_local["ore"] - clay_robot_cost

                f_decision(m + 1, minerals_local, robots_local)
            }
            else if (options[i] == 2) {
                minerals_local <- minerals
                robots_local <- robots

                robots_local["ore"] <- robots_local["ore"] + 1
                minerals_local["ore"] <- minerals_local["ore"] - ore_robot_cost

                f_decision(m + 1, minerals_local, robots_local)
            }

        }


    }

}

v <- c()
# for (i in 2) {
for (i in seq_along(input)) {

    blueprint <- input[[i]]
    blueprint <- as.integer(blueprint)

    ore_robot_cost <- blueprint[2]

    clay_robot_cost <- blueprint[3]

    obsidian_robot_cost_ore <- blueprint[4]
    obsidian_robot_cost_clay <- blueprint[5]

    geode_robot_cost_ore <- blueprint[6]
    geode_robot_cost_obsidian <- blueprint[7]

    max_ore_cost <- max(blueprint[c(2, 4, 6)])


    minerals_global <- c(ore = 0, clay = 0, obsidian = 0, geode = 0)
    minerals_global
    robots_global <- c(ore = 1, clay = 0, obsidian = 0, geode = 0)
    robots_global

    if (i == 1) geode_list <- list(10)
    else if (i == 2) geode_list <- list(13)
    else if (i == 3) geode_list <- list(21)
    full_list <- list()


    f_decision(1, minerals_global, robots_global)
    v[i] <- max(unlist(geode_list))

}


v
max(unlist(geode_list))
length(geode_list)
head(full_list)
tail(full_list)

v[1] * v[2] * v[3]
