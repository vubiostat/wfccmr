library(Wfccm)

# asam > 4 5 6
(s <- Criteria("asam", ">", 4:6))
as.Criteria("asam > 4 5 6")
# wga > 0.1 0.3
(w <- Criteria("wga", ">", c(0.1, 0.3)))
as.Criteria("wga > .1 .3")
# prob.t < 1e-04
(t <- Criteria("prob.t", "<", 0.0001))
as.Criteria("prob.t < 1e-4")
# prob.d < 1e-04
(d <- Criteria("prob.d", "<", 0.0001))
as.Criteria("prob.d < 1e-4")

# asam > 4 5 6
# wga > 0.1 0.3
# prob.t < 1e-04
(crits <- c(s,w,t))

# TRUE
is.Criteria(crits)

# asam > 4 5 6
crits[1]

# wga > 0.1 0.3
crits["wga"]

# asam > 6
crits[1,3]

# asam > 6
# wga > 0.3
# prob.t < 1e-04
crits[,3:1]
# asam > 6
# wga > 0.3
# prob.t < 1e-04
crits[[6]]

crits[4] <- d
# asam > 4 5 6
# wga > 0.1 0.3
# prob.t < 1e-04
# prob.d < 1e-04
crits

crits[4,1] <- 0.0005
# asam > 4 5 6
# wga > 0.1 0.3
# prob.t < 1e-04
# prob.d < 5e-04
crits
