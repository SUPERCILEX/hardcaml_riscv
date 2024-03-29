route_design -ultrathreads -directive AggressiveExplore -tns_cleanup

phys_opt_design -directive AggressiveExplore
phys_opt_design -directive AggressiveFanoutOpt
phys_opt_design -directive ExploreWithAggressiveHoldFix
phys_opt_design -directive AlternateFlowWithRetiming
phys_opt_design -directive AlternateReplication
phys_opt_design -directive AggressiveExplore

place_design -post_place_opt
phys_opt_design -directive AggressiveExplore
phys_opt_design -directive AggressiveFanoutOpt
phys_opt_design -directive ExploreWithAggressiveHoldFix
phys_opt_design -directive AlternateFlowWithRetiming
phys_opt_design -directive AlternateReplication
phys_opt_design -directive AggressiveExplore
route_design -ultrathreads -directive AggressiveExplore -tns_cleanup

phys_opt_design -directive AggressiveExplore
phys_opt_design -directive AggressiveFanoutOpt
phys_opt_design -directive ExploreWithAggressiveHoldFix
phys_opt_design -directive AlternateFlowWithRetiming
phys_opt_design -directive AlternateReplication
phys_opt_design -directive AggressiveExplore
