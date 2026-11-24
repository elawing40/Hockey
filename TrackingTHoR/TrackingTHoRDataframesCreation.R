#Prediction Models at Different Times
library(dplyr)
library(stringr)
library(purrr)
library(fastDummies)
library(pROC)

#Event Data with Time to Next Goal
HALOEventsTimeToNextTeamGoal

HALOEventsTimeToNextTeamGoalHasTracking <- HALOEventsTimeToNextTeamGoal %>%
  filter(has_tracking_data == 1)

#Tracking Data
HALOTracking

#Cleaning Data 
#Filter out all penalties, penalties drawn, shootout, penalty shot, failed pass location, goal, assist, icing, offside events
#Data for assist is stored in a pass and the data for the goal is stored in a shot
HALOEventsToFilterOut <- c("assist", "failedpasslocation", "goal", "icing", "offside", "penalty", "penaltydrawn", "pscarry", 
                           "pslpr", "socarry", "socheck", "sogoal", "solpr", "sopuckprotection", "soshot")

HALOEventsTimeToGoalHasTrackingClean <- HALOEventsTimeToNextTeamGoalHasTracking %>%
  filter(!(event_type %in% HALOEventsToFilterOut))

#Joining the Tracking Data to the Event Data
HALOEventsTimeToNextTeamGoalTrackingDataJoin <- HALOEventsTimeToGoalHasTrackingClean %>%
  left_join(HALOTracking, by = c("game_id", "sl_event_id")) %>%
  left_join(HALOPlayers %>% select(player_id, primary_position), 
            by = c("player_id.y" = "player_id")) %>%
  filter(primary_position %in% c("F","D","RW","LW","C",NA))


HALOEventsTimeToNextTeamGoalTrackingNetSide <- HALOEventsTimeToNextTeamGoalTrackingDataJoin %>%
  left_join(HALOGames %>% select(game_id, home_start_net, home_team, home_team_id), by = "game_id") %>%
  mutate(HomeAttackingNet = case_when(
    period == 1 & home_start_net == "pos_x" ~ "pos_x",
    period == 1 & home_start_net == "neg_x" ~ "neg_x",
    period == 2 & home_start_net == "pos_x" ~ "neg_x",
    period == 2 & home_start_net == "neg_x" ~ "pos_x",
    period == 3 & home_start_net == "pos_x" ~ "pos_x",
    period == 3 & home_start_net == "neg_x" ~ "neg_x",
    period == 4 & home_start_net == "pos_x" ~ "neg_x",
    period == 4 & home_start_net == "neg_x" ~ "pos_x"
  ),
  HomeAttackingNetXCoord = ifelse(HomeAttackingNet=="pos_x", 89, -89),
  HomeAttackingNetYCoord = 0,
  AwayAttackingNetXCoord = ifelse(HomeAttackingNet=="pos_x", -89, 89),
  AwayAttackingNetYCoord = 0)

HALOEventsTrackingDistancesToPuck <- HALOEventsTimeToNextTeamGoalTrackingNetSide %>%
  mutate(
    EventDistanceToHomeNet = sqrt((x - HomeAttackingNetXCoord)^2 + (y - HomeAttackingNetYCoord)^2),
    EventDistanceToAwayNet = sqrt((x - AwayAttackingNetXCoord)^2 + (y - AwayAttackingNetYCoord)^2),
    EventAngleToHomeNet = abs(atan2(HomeAttackingNetYCoord-y, HomeAttackingNetXCoord-x) * (180/pi)),
    EventAngleToAwayNet = abs(atan2(AwayAttackingNetYCoord-y, AwayAttackingNetXCoord-x) * (180/pi)),
    DistanceToEvent = sqrt((tracking_x - x)^2 + (tracking_y - y)^2),
    PlayerSpeed = sqrt(tracking_vel_x^2 + tracking_vel_y^2),
    SpeedTowardsEvent = (tracking_vel_x * (x - tracking_x) +
                           tracking_vel_y * (y - tracking_y)) / 
      (DistanceToEvent + 0.00001),
    DistanceToHomeAttackingNet = sqrt((tracking_x - HomeAttackingNetXCoord)^2 + (tracking_y - HomeAttackingNetYCoord)^2),
    DistanceToAwayAttackingNet = sqrt((tracking_x - AwayAttackingNetXCoord)^2 + (tracking_y - AwayAttackingNetYCoord)^2),
    AngleToHomeAttackingNet = abs(atan2(HomeAttackingNetYCoord-tracking_y, HomeAttackingNetXCoord-tracking_x) * (180/pi)),
    AngleToAwayAttackingNet = abs(atan2(AwayAttackingNetYCoord-tracking_y, AwayAttackingNetXCoord-tracking_x) * (180/pi)),
    AngleToEvent = abs(atan2(y-tracking_y, x-tracking_x) * (180/pi))) %>%
  group_by(game_id, sl_event_id) %>%
  mutate(
    DistanceToClosestOpponent = map_dbl(seq_len(n()), function(i){
      current_team <- team_id.y[i]
      current_x <- tracking_x[i]
      current_y <- tracking_y[i]
      
      opp_mask <- team_id.y != current_team
      
      if(!any(opp_mask)) return(NA_real_)
      
      opp_x <- tracking_x[opp_mask]
      opp_y <- tracking_y[opp_mask]
      min(sqrt((current_x - opp_x)^2 + (current_y - opp_y)^2))
    })
  ) %>%
  ungroup()


HALOEventsTrackingDistancesToPuckEventSummary <- HALOEventsTrackingDistancesToPuck %>%
  filter(!is.na(player_id.y), !is.na(DistanceToEvent)) %>% 
  group_by(game_id, sl_event_id) %>%
  summarise(
    game_id = first(game_id),
    period = first(period),
    period_time = first(period_time),
    game_stint = first(game_stint),
    sl_event_id = first(sl_event_id),
    sl_xg_all_shots = first(sl_xg_all_shots),
    sequence_id = first(sequence_id),
    StrengthState = first(StrengthState),
    HomeTeamGoals = first(HomeTeamGoals),
    RoadTeamGoals = first(RoadTeamGoals),
    HomeTeamScoreDiff = first(HomeTeamScoreDiff),
    EventPlayerID = first(player_id.x),
    EventPlayerName = first(player_name.x),
    EventTeam = first(team),
    EventTeamID = first(team_id.x),
    OppTeam = first(opp_team),
    OppTeamID = first(opp_team_id),
    home_team_id = first(home_team_id),
    event_type = first(event_type),
    outcome = first(outcome),
    flags = first(flags),
    description = first(description),
    detail = first(detail),
    EventLocation_X = first(x),
    EventLocation_Y = first(y),
    EventLocation_XAdj = first(x_adj),
    EventLocation_YAdj = first(y_adj),
    home_team = first(home_team.x),
    away_team = first(away_team),
    isHomeEvent = first(ifelse(team == home_team.x, 1, 0)),
    TimeToNextTeamGoal = first(TimeToNextTeamGoal),
    TeamGoalWithinNext3Sec = first(TeamGoalWithinNext3Sec),
    TeamGoalWithinNext5Sec = first(TeamGoalWithinNext5Sec),
    TeamGoalWithinNext7Sec = first(TeamGoalWithinNext7Sec),
    TeamGoalWithinNext10Sec = first(TeamGoalWithinNext10Sec),
    TeamGoalWithinNext11Sec = first(TeamGoalWithinNext11Sec),
    TeamGoalWithinNext12Sec = first(TeamGoalWithinNext12Sec),
    TeamGoalWithinNext15Sec = first(TeamGoalWithinNext15Sec),
    TeamGoalWithinNext17Sec = first(TeamGoalWithinNext17Sec),
    TeamGoalWithinNext20Sec = first(TeamGoalWithinNext20Sec),
    TeamGoalWithinNext25Sec = first(TeamGoalWithinNext25Sec),
    TeamGoalWithinNext30Sec = first(TeamGoalWithinNext30Sec),
    home_start_net = first(home_start_net),
    HomeAttackingNet = first(HomeAttackingNet),
    EventDistanceToHomeNet = first(EventDistanceToHomeNet),
    EventDistanceToAwayNet = first(EventDistanceToAwayNet),
    EventAngleToHomeNet = first(EventAngleToHomeNet),
    EventAngleToAwayNet = first(EventAngleToAwayNet),
    HomePlayer1DistanceToEvent = {
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) > 0) min(dists, na.rm = TRUE) else NA_real_
    },
    HomePlayer1Speed = {
      if(any(team_id.y == home_team_id)) {
        PlayerSpeed[team_id.y == home_team_id][which.min(DistanceToEvent[team_id.y == home_team_id])]
      } else NA_real_
    },
    HomePlayer1SpeedTowardEvent = {
      if(any(team_id.y == home_team_id)) {
        SpeedTowardsEvent[team_id.y == home_team_id][which.min(DistanceToEvent[team_id.y == home_team_id])]
      } else NA_real_
    },
    HomePlayer1DistanceToHomeAttckingNet = {
      if(any(team_id.y == home_team_id)) {
        DistanceToHomeAttackingNet[team_id.y == home_team_id][which.min(DistanceToEvent[team_id.y == home_team_id])]
      } else NA_real_
    },
    HomePlayer1DistanceToAwayAttackingNet = {
      if(any(team_id.y == home_team_id)) {
        DistanceToAwayAttackingNet[team_id.y == home_team_id][which.min(DistanceToEvent[team_id.y == home_team_id])]
      } else NA_real_
    },
    HomePlayer1AngleToHomeAttackingNet = {
      if(any(team_id.y == home_team_id)) {
        AngleToHomeAttackingNet[team_id.y == home_team_id][which.min(DistanceToEvent[team_id.y == home_team_id])]
      } else NA_real_
    },
    HomePlayer1AngleToAwayAttackingNet = {
      if(any(team_id.y == home_team_id)) {
        AngleToAwayAttackingNet[team_id.y == home_team_id][which.min(DistanceToEvent[team_id.y == home_team_id])]
      } else NA_real_
    },
    HomePlayer1AngleToEvent = {
      if(any(team_id.y == home_team_id)) {
        AngleToEvent[team_id.y == home_team_id][which.min(DistanceToEvent[team_id.y == home_team_id])]
      } else NA_real_
    },
    HomePlayer1ClosestOpponent = {
      if(any(team_id.y == home_team_id)) {
        DistanceToClosestOpponent[team_id.y == home_team_id][which.min(DistanceToEvent[team_id.y == home_team_id])]
      } else NA_real_
    },
    HomePlayer1TrackingID = {
      if(any(team_id.y == home_team_id)) {
        player_id.y[team_id.y == home_team_id][which.min(DistanceToEvent[team_id.y == home_team_id])]
      } else NA_character_
    },
    HomePlayer2DistanceToEvent = {
      dists <- sort(DistanceToEvent[team_id.y == home_team_id])
      if(length(dists) >= 2) dists[2] else NA_real_
    },
    HomePlayer2Speed = {
      speeds <- PlayerSpeed[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 2) speeds[order(dists)[2]] else NA_real_
    },
    HomePlayer2SpeedTowardEvent = {
      speeds <- SpeedTowardsEvent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 2) speeds[order(dists)[2]] else NA_real_
    },
    HomePlayer2DistanceToHomeAttckingNet = {
      DistsToHomeNet <- DistanceToHomeAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 2) DistsToHomeNet[order(dists)[2]] else NA_real_
    },
    HomePlayer2DistanceToAwayAttackingNet = {
      DistsToAwayNet <- DistanceToAwayAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 2) DistsToAwayNet[order(dists)[2]] else NA_real_
    },
    HomePlayer2AngleToHomeAttackingNet = {
      AngleToHomeNet <- AngleToHomeAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 2) AngleToHomeNet[order(dists)[2]] else NA_real_
    },
    HomePlayer2AngleToAwayAttackingNet = {
      AngleToAwayNet <- AngleToAwayAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 2) AngleToAwayNet[order(dists)[2]] else NA_real_
    },
    HomePlayer2AngleToEvent = {
      AngleToEvent <- AngleToEvent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 2) AngleToEvent[order(dists)[2]] else NA_real_
    },
    HomePlayer2ClosestOpponent = {
      DistanceToClosestOpponent <- DistanceToClosestOpponent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 2) DistanceToClosestOpponent[order(dists)[2]] else NA_real_
    },
    HomePlayer2TrackingID = {
      PlayerID <- player_id.y[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 2) PlayerID[order(dists)[2]] else NA_character_
    },
    HomePlayer3DistanceToEvent = {
      dists <- sort(DistanceToEvent[team_id.y == home_team_id])
      if(length(dists) >= 3) dists[3] else NA_real_
    },
    HomePlayer3Speed = {
      speeds <- PlayerSpeed[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 3) speeds[order(dists)[3]] else NA_real_
    },
    HomePlayer3SpeedTowardEvent = {
      speeds <- SpeedTowardsEvent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 3) speeds[order(dists)[3]] else NA_real_
    },
    HomePlayer3DistanceToHomeAttckingNet = {
      DistsToHomeNet <- DistanceToHomeAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 3) DistsToHomeNet[order(dists)[3]] else NA_real_
    },
    HomePlayer3DistanceToAwayAttackingNet = {
      DistsToAwayNet <- DistanceToAwayAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 3) DistsToAwayNet[order(dists)[3]] else NA_real_
    },
    HomePlayer3AngleToHomeAttackingNet = {
      AngleToHomeNet <- AngleToHomeAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 3) AngleToHomeNet[order(dists)[3]] else NA_real_
    },
    HomePlayer3AngleToAwayAttackingNet = {
      AngleToAwayNet <- AngleToAwayAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 3) AngleToAwayNet[order(dists)[3]] else NA_real_
    },
    HomePlayer3AngleToEvent = {
      AngleToEvent <- AngleToEvent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 3) AngleToEvent[order(dists)[3]] else NA_real_
    },
    HomePlayer3ClosestOpponent = {
      DistanceToClosestOpponent <- DistanceToClosestOpponent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 3) DistanceToClosestOpponent[order(dists)[3]] else NA_real_
    },
    HomePlayer3TrackingID = {
      PlayerID <- player_id.y[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 3) PlayerID[order(dists)[3]] else NA_character_
    },
    HomePlayer4DistanceToEvent = {
      dists <- sort(DistanceToEvent[team_id.y == home_team_id])
      if(length(dists) >= 4) dists[4] else NA_real_
    },
    HomePlayer4Speed = {
      speeds <- PlayerSpeed[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 4) speeds[order(dists)[4]] else NA_real_
    },
    HomePlayer4SpeedTowardEvent = {
      speeds <- SpeedTowardsEvent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 4) speeds[order(dists)[4]] else NA_real_
    },
    HomePlayer4DistanceToHomeAttckingNet = {
      DistsToHomeNet <- DistanceToHomeAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 4) DistsToHomeNet[order(dists)[4]] else NA_real_
    },
    HomePlayer4DistanceToAwayAttackingNet = {
      DistsToAwayNet <- DistanceToAwayAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 4) DistsToAwayNet[order(dists)[4]] else NA_real_
    },
    HomePlayer4AngleToHomeAttackingNet = {
      AngleToHomeNet <- AngleToHomeAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 4) AngleToHomeNet[order(dists)[4]] else NA_real_
    },
    HomePlayer4AngleToAwayAttackingNet = {
      AngleToAwayNet <- AngleToAwayAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 4) AngleToAwayNet[order(dists)[4]] else NA_real_
    },
    HomePlayer4AngleToEvent = {
      AngleToEvent <- AngleToEvent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 4) AngleToEvent[order(dists)[4]] else NA_real_
    },
    HomePlayer4ClosestOpponent = {
      DistanceToClosestOpponent <- DistanceToClosestOpponent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 4) DistanceToClosestOpponent[order(dists)[4]] else NA_real_
    },
    HomePlayer4TrackingID = {
      PlayerID <- player_id.y[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 4) PlayerID[order(dists)[4]] else NA_character_
    },
    HomePlayer5DistanceToEvent = {
      dists <- sort(DistanceToEvent[team_id.y == home_team_id])
      if(length(dists) >= 5) dists[5] else NA_real_
    },
    HomePlayer5Speed = {
      speeds <- PlayerSpeed[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 5) speeds[order(dists)[5]] else NA_real_
    },
    HomePlayer5SpeedTowardEvent = {
      speeds <- SpeedTowardsEvent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 5) speeds[order(dists)[5]] else NA_real_
    },
    HomePlayer5DistanceToHomeAttckingNet = {
      DistsToHomeNet <- DistanceToHomeAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 5) DistsToHomeNet[order(dists)[5]] else NA_real_
    },
    HomePlayer5DistanceToAwayAttackingNet = {
      DistsToAwayNet <- DistanceToAwayAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 5) DistsToAwayNet[order(dists)[5]] else NA_real_
    },
    HomePlayer5AngleToHomeAttackingNet = {
      AngleToHomeNet <- AngleToHomeAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 5) AngleToHomeNet[order(dists)[5]] else NA_real_
    },
    HomePlayer5AngleToAwayAttackingNet = {
      AngleToAwayNet <- AngleToAwayAttackingNet[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 5) AngleToAwayNet[order(dists)[5]] else NA_real_
    },
    HomePlayer5AngleToEvent = {
      AngleToEvent <- AngleToEvent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 5) AngleToEvent[order(dists)[5]] else NA_real_
    },
    HomePlayer5ClosestOpponent = {
      DistanceToClosestOpponent <- DistanceToClosestOpponent[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 5) DistanceToClosestOpponent[order(dists)[5]] else NA_real_
    },
    HomePlayer5TrackingID = {
      PlayerID <- player_id.y[team_id.y == home_team_id]
      dists <- DistanceToEvent[team_id.y == home_team_id]
      if(length(dists) >= 5) PlayerID[order(dists)[5]] else NA_character_
    },
    AwayPlayer1DistanceToEvent = {
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) > 0) min(dists, na.rm = TRUE) else NA_real_
    },
    AwayPlayer1Speed = {
      if(any(team_id.y != home_team_id)) {
        PlayerSpeed[team_id.y != home_team_id][which.min(DistanceToEvent[team_id.y != home_team_id])]
      } else NA_real_
    },
    AwayPlayer1SpeedTowardEvent = {
      if(any(team_id.y != home_team_id)) {
        SpeedTowardsEvent[team_id.y != home_team_id][which.min(DistanceToEvent[team_id.y != home_team_id])]
      } else NA_real_
    },
    AwayPlayer1DistanceToHomeAttckingNet = {
      if(any(team_id.y != home_team_id)) {
        DistanceToHomeAttackingNet[team_id.y != home_team_id][which.min(DistanceToEvent[team_id.y != home_team_id])]
      } else NA_real_
    },
    AwayPlayer1DistanceToAwayAttackingNet = {
      if(any(team_id.y != home_team_id)) {
        DistanceToAwayAttackingNet[team_id.y != home_team_id][which.min(DistanceToEvent[team_id.y != home_team_id])]
      } else NA_real_
    },
    AwayPlayer1AngleToHomeAttackingNet = {
      if(any(team_id.y != home_team_id)) {
        AngleToHomeAttackingNet[team_id.y != home_team_id][which.min(DistanceToEvent[team_id.y != home_team_id])]
      } else NA_real_
    },
    AwayPlayer1AngleToAwayAttackingNet = {
      if(any(team_id.y != home_team_id)) {
        AngleToAwayAttackingNet[team_id.y != home_team_id][which.min(DistanceToEvent[team_id.y != home_team_id])]
      } else NA_real_
    },
    AwayPlayer1AngleToEvent = {
      if(any(team_id.y != home_team_id)) {
        AngleToEvent[team_id.y != home_team_id][which.min(DistanceToEvent[team_id.y != home_team_id])]
      } else NA_real_
    },
    AwayPlayer1ClosestOpponent = {
      if(any(team_id.y != home_team_id)) {
        DistanceToClosestOpponent[team_id.y != home_team_id][which.min(DistanceToEvent[team_id.y != home_team_id])]
      } else NA_real_
    },
    AwayPlayer1TrackingID = {
      if(any(team_id.y != home_team_id)) {
        player_id.y[team_id.y != home_team_id][which.min(DistanceToEvent[team_id.y != home_team_id])]
      } else NA_character_
    },
    AwayPlayer2DistanceToEvent = {
      dists <- sort(DistanceToEvent[team_id.y != home_team_id])
      if(length(dists) >= 2) dists[2] else NA_real_
    },
    AwayPlayer2Speed = {
      speeds <- PlayerSpeed[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 2) speeds[order(dists)[2]] else NA_real_
    },
    AwayPlayer2SpeedTowardEvent = {
      speeds <- SpeedTowardsEvent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 2) speeds[order(dists)[2]] else NA_real_
    },
    AwayPlayer2DistanceToHomeAttckingNet = {
      DistsToHomeNet <- DistanceToHomeAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 2) DistsToHomeNet[order(dists)[2]] else NA_real_
    },
    AwayPlayer2DistanceToAwayAttackingNet = {
      DistsToAwayNet <- DistanceToAwayAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 2) DistsToAwayNet[order(dists)[2]] else NA_real_
    },
    AwayPlayer2AngleToHomeAttackingNet = {
      AngleToHomeNet <- AngleToHomeAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 2) AngleToHomeNet[order(dists)[2]] else NA_real_
    },
    AwayPlayer2AngleToAwayAttackingNet = {
      AngleToAwayNet <- AngleToAwayAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 2) AngleToAwayNet[order(dists)[2]] else NA_real_
    },
    AwayPlayer2AngleToEvent = {
      AngleToEvent <- AngleToEvent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 2) AngleToEvent[order(dists)[2]] else NA_real_
    },
    AwayPlayer2ClosestOpponent = {
      DistanceToClosestOpponent <- DistanceToClosestOpponent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 2) DistanceToClosestOpponent[order(dists)[2]] else NA_real_
    },
    AwayPlayer2TrackingID = {
      PlayerID <- player_id.y[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 2) PlayerID[order(dists)[2]] else NA_character_
    },
    AwayPlayer3DistanceToEvent = {
      dists <- sort(DistanceToEvent[team_id.y != home_team_id])
      if(length(dists) >= 3) dists[3] else NA_real_
    },
    AwayPlayer3Speed = {
      speeds <- PlayerSpeed[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 3) speeds[order(dists)[3]] else NA_real_
    },
    AwayPlayer3SpeedTowardEvent = {
      speeds <- SpeedTowardsEvent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 3) speeds[order(dists)[3]] else NA_real_
    },
    AwayPlayer3DistanceToHomeAttckingNet = {
      DistsToHomeNet <- DistanceToHomeAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 3) DistsToHomeNet[order(dists)[3]] else NA_real_
    },
    AwayPlayer3DistanceToAwayAttackingNet = {
      DistsToAwayNet <- DistanceToAwayAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 3) DistsToAwayNet[order(dists)[3]] else NA_real_
    },
    AwayPlayer3AngleToHomeAttackingNet = {
      AngleToHomeNet <- AngleToHomeAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 3) AngleToHomeNet[order(dists)[3]] else NA_real_
    },
    AwayPlayer3AngleToAwayAttackingNet = {
      AngleToAwayNet <- AngleToAwayAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 3) AngleToAwayNet[order(dists)[3]] else NA_real_
    },
    AwayPlayer3AngleToEvent = {
      AngleToEvent <- AngleToEvent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 3) AngleToEvent[order(dists)[3]] else NA_real_
    },
    AwayPlayer3ClosestOpponent = {
      DistanceToClosestOpponent <- DistanceToClosestOpponent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 3) DistanceToClosestOpponent[order(dists)[3]] else NA_real_
    },
    AwayPlayer3TrackingID = {
      PlayerID <- player_id.y[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 3) PlayerID[order(dists)[3]] else NA_character_
    },
    AwayPlayer4DistanceToEvent = {
      dists <- sort(DistanceToEvent[team_id.y != home_team_id])
      if(length(dists) >= 4) dists[4] else NA_real_
    },
    AwayPlayer4Speed = {
      speeds <- PlayerSpeed[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 4) speeds[order(dists)[4]] else NA_real_
    },
    AwayPlayer4SpeedTowardEvent = {
      speeds <- SpeedTowardsEvent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 4) speeds[order(dists)[4]] else NA_real_
    },
    AwayPlayer4DistanceToHomeAttckingNet = {
      DistsToHomeNet <- DistanceToHomeAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 4) DistsToHomeNet[order(dists)[4]] else NA_real_
    },
    AwayPlayer4DistanceToAwayAttackingNet = {
      DistsToAwayNet <- DistanceToAwayAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 4) DistsToAwayNet[order(dists)[4]] else NA_real_
    },
    AwayPlayer4AngleToHomeAttackingNet = {
      AngleToHomeNet <- AngleToHomeAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 4) AngleToHomeNet[order(dists)[4]] else NA_real_
    },
    AwayPlayer4AngleToAwayAttackingNet = {
      AngleToAwayNet <- AngleToAwayAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 4) AngleToAwayNet[order(dists)[4]] else NA_real_
    },
    AwayPlayer4AngleToEvent = {
      AngleToEvent <- AngleToEvent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 4) AngleToEvent[order(dists)[4]] else NA_real_
    },
    AwayPlayer4ClosestOpponent = {
      DistanceToClosestOpponent <- DistanceToClosestOpponent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 4) DistanceToClosestOpponent[order(dists)[4]] else NA_real_
    },
    AwayPlayer4TrackingID = {
      PlayerID <- player_id.y[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 4) PlayerID[order(dists)[4]] else NA_character_
    },
    AwayPlayer5DistanceToEvent = {
      dists <- sort(DistanceToEvent[team_id.y != home_team_id])
      if(length(dists) >= 5) dists[5] else NA_real_
    },
    AwayPlayer5Speed = {
      speeds <- PlayerSpeed[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 5) speeds[order(dists)[5]] else NA_real_
    },
    AwayPlayer5SpeedTowardEvent = {
      speeds <- SpeedTowardsEvent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 5) speeds[order(dists)[5]] else NA_real_
    },
    AwayPlayer5DistanceToHomeAttckingNet = {
      DistsToHomeNet <- DistanceToHomeAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 5) DistsToHomeNet[order(dists)[5]] else NA_real_
    },
    AwayPlayer5DistanceToAwayAttackingNet = {
      DistsToAwayNet <- DistanceToAwayAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 5) DistsToAwayNet[order(dists)[5]] else NA_real_
    },
    AwayPlayer5AngleToHomeAttackingNet = {
      AngleToHomeNet <- AngleToHomeAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 5) AngleToHomeNet[order(dists)[5]] else NA_real_
    },
    AwayPlayer5AngleToAwayAttackingNet = {
      AngleToAwayNet <- AngleToAwayAttackingNet[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 5) AngleToAwayNet[order(dists)[5]] else NA_real_
    },
    AwayPlayer5AngleToEvent = {
      AngleToEvent <- AngleToEvent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 5) AngleToEvent[order(dists)[5]] else NA_real_
    },
    AwayPlayer5ClosestOpponent = {
      DistanceToClosestOpponent <- DistanceToClosestOpponent[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 5) DistanceToClosestOpponent[order(dists)[5]] else NA_real_
    },
    AwayPlayer5TrackingID = {
      PlayerID <- player_id.y[team_id.y != home_team_id]
      dists <- DistanceToEvent[team_id.y != home_team_id]
      if(length(dists) >= 5) PlayerID[order(dists)[5]] else NA_character_
    },
    .groups = "drop"
    )
    
bad <- HALOEventsTrackingDistancesToPuckEventSummary %>%
  filter(is.na(HomePlayer1TrackingID), !is.na(HomePlayer1DistanceToEvent)) %>%
  slice(1:3) %>% select(game_id, sl_event_id)

HALOEventsTrackingDistancesToPuck %>%
  semi_join(bad, by = c("game_id", "sl_event_id")) %>%
  select(game_id, sl_event_id, player_id.y, team_id.y, primary_position, DistanceToEvent) %>%
  arrange(game_id, sl_event_id, DistanceToEvent) %>%
  print(n = 40)

HALOEventsTrackingDistancesToPuck %>%
  summarise(na_player = mean(is.na(player_id.y)),
            na_team   = mean(is.na(team_id.y)),
            na_pos    = mean(is.na(primary_position)))

for (side in c("Home", "Away")) {
  for (k in 1:5) {
    id_col   <- paste0(side, "Player", k, "TrackingID")
    dist_col <- paste0(side, "Player", k, "DistanceToEvent")
    bad_n <- sum(is.na(HALOEventsTrackingDistancesToPuckEventSummary[[id_col]]) &
                   !is.na(HALOEventsTrackingDistancesToPuckEventSummary[[dist_col]]))
    cat(id_col, ": ", bad_n, "\n")
  }
}

HALOEventsTrackingDistancesToPuckEventSummaryWithZone <- HALOEventsTrackingDistancesToPuckEventSummary %>%
  mutate(ZoneByHomePerpsective = case_when(
    HomeAttackingNet == "pos_x" & EventLocation_X > 25  ~ "DZ", 
    HomeAttackingNet == "pos_x" & EventLocation_X < -25 ~ "OZ",
    HomeAttackingNet == "neg_x" & EventLocation_X < -25 ~ "DZ",
    HomeAttackingNet == "neg_x" & EventLocation_X >  25 ~ "OZ",
    abs(EventLocation_X) <= 25 ~ "NZ"
  ),
  ZoneByAwayPerpsective = case_when(ZoneByHomePerpsective == "OZ" ~ "DZ",
                                    ZoneByHomePerpsective == "DZ" ~ "OZ",
                                    ZoneByHomePerpsective == "NZ" ~ "NZ"),
  EventTypeWithZone = ifelse(isHomeEvent==1, paste0(event_type, "_", ZoneByHomePerpsective), paste0(event_type, "_", ZoneByAwayPerpsective)),
  ShotType = case_when(
    event_type != "shot" ~ NA_character_,
    grepl("deflection", flags, ignore.case = TRUE) ~ "deflected",
    grepl("screened", flags, ignore.case = TRUE) ~ stringr::word(gsub("screened[, ]+", "", flags), 1, sep = "[ ,]+"),
    TRUE ~ stringr::word(flags, 1, sep = "[ ,]+")),
  SuccessfulEventFlag = ifelse(outcome=="successful", 1, 0))
#the deflection event and the actual deflected shots have different locations on different events, so both will be kept in the dataset
#deflected shots have NA for the xG value, deflections have an associated xG value

unique(HALOEventsTrackingDistancesToPuckEventSummaryWithZone$ShotType)
unique(HALOEventsTrackingDistancesToPuckEventSummaryWithZone$EventTypeWithZone)


HALOEventsTrackingDistancesToPuckEventSummaryWithZoneSetOffenseDefense <- HALOEventsTrackingDistancesToPuckEventSummaryWithZone %>%
  mutate(
    OffensiveTeam = case_when(
      event_type %in% OffensiveEventTypes ~ EventTeam,
      event_type %in% DefensiveEventTypes ~ OppTeam,
      event_type == "faceoff" & outcome %in% c("successful", "undetermined")~ EventTeam,
      event_type == "faceoff" & outcome == "failed" ~ OppTeam,
      TRUE ~ NA_character_
    ),
    isHomeOffense = case_when(
      is.na(OffensiveTeam) ~ NA_integer_,
      OffensiveTeam == home_team ~ 1L,
      OffensiveTeam == away_team ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!(event_type == "faceoff" & outcome %in% c("failed", "undetermined")))

#Preparing the dataframes for the XGBoost model
HALOEventsTrackingDistancesToPuckEventSummaryWithZoneDummys <- HALOEventsTrackingDistancesToPuckEventSummaryWithZoneSetOffenseDefense %>%
  dummy_cols(
    select_columns = c("EventTypeWithZone", "ShotType"),
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE,
    ignore_na = TRUE
  ) %>%
  mutate(
    OffensiveZone = ifelse(ZoneByHomePerpsective == "OZ", 1, 0),
    DefensiveZone = ifelse(ZoneByHomePerpsective == "DZ", 1, 0)
  ) %>%
  select(-ZoneByHomePerpsective, -ZoneByAwayPerpsective)


HALOEventsWithTrackingForXGBoost <- HALOEventsTrackingDistancesToPuckEventSummaryWithZoneDummys %>%
  select(game_id, sl_event_id,
         TeamGoalWithinNext3Sec, TeamGoalWithinNext5Sec,
         TeamGoalWithinNext7Sec, TeamGoalWithinNext10Sec,
         TeamGoalWithinNext11Sec, TeamGoalWithinNext12Sec,
         TeamGoalWithinNext15Sec, TeamGoalWithinNext17Sec,
         TeamGoalWithinNext20Sec, TeamGoalWithinNext25Sec,
         TeamGoalWithinNext30Sec, isHomeEvent,
         sl_xg_all_shots,
         EventDistanceToHomeNet, EventDistanceToAwayNet,
         EventAngleToHomeNet, EventAngleToAwayNet,
         HomePlayer1DistanceToEvent, HomePlayer1Speed, 
         HomePlayer1SpeedTowardEvent, HomePlayer1DistanceToHomeAttckingNet,
         HomePlayer1DistanceToAwayAttackingNet, HomePlayer1AngleToHomeAttackingNet,
         HomePlayer1AngleToAwayAttackingNet, HomePlayer1AngleToEvent, HomePlayer1ClosestOpponent,
         HomePlayer2DistanceToEvent, HomePlayer2Speed, 
         HomePlayer2SpeedTowardEvent, HomePlayer2DistanceToHomeAttckingNet,
         HomePlayer2DistanceToAwayAttackingNet, HomePlayer2AngleToHomeAttackingNet,
         HomePlayer2AngleToAwayAttackingNet, HomePlayer2AngleToEvent, HomePlayer2ClosestOpponent,
         HomePlayer3DistanceToEvent, HomePlayer3Speed, 
         HomePlayer3SpeedTowardEvent, HomePlayer3DistanceToHomeAttckingNet,
         HomePlayer3DistanceToAwayAttackingNet, HomePlayer3AngleToHomeAttackingNet,
         HomePlayer3AngleToAwayAttackingNet, HomePlayer3AngleToEvent, HomePlayer3ClosestOpponent,
         HomePlayer4DistanceToEvent, HomePlayer4Speed, 
         HomePlayer4SpeedTowardEvent, HomePlayer4DistanceToHomeAttckingNet,
         HomePlayer4DistanceToAwayAttackingNet, HomePlayer4AngleToHomeAttackingNet,
         HomePlayer4AngleToAwayAttackingNet, HomePlayer4AngleToEvent, HomePlayer4ClosestOpponent,
         HomePlayer5DistanceToEvent, HomePlayer5Speed, 
         HomePlayer5SpeedTowardEvent, HomePlayer5DistanceToHomeAttckingNet,
         HomePlayer5DistanceToAwayAttackingNet, HomePlayer5AngleToHomeAttackingNet,
         HomePlayer5AngleToAwayAttackingNet, HomePlayer5AngleToEvent, HomePlayer5ClosestOpponent,
         AwayPlayer1DistanceToEvent, AwayPlayer1Speed,
         AwayPlayer1SpeedTowardEvent, AwayPlayer1DistanceToHomeAttckingNet,
         AwayPlayer1DistanceToAwayAttackingNet, AwayPlayer1AngleToHomeAttackingNet,
         AwayPlayer1AngleToAwayAttackingNet, AwayPlayer1AngleToEvent, AwayPlayer1ClosestOpponent,
         AwayPlayer2DistanceToEvent, AwayPlayer2Speed,
         AwayPlayer2SpeedTowardEvent, AwayPlayer2DistanceToHomeAttckingNet,
         AwayPlayer2DistanceToAwayAttackingNet, AwayPlayer2AngleToHomeAttackingNet,
         AwayPlayer2AngleToAwayAttackingNet, AwayPlayer2AngleToEvent, AwayPlayer2ClosestOpponent,
         AwayPlayer3DistanceToEvent, AwayPlayer3Speed,
         AwayPlayer3SpeedTowardEvent, AwayPlayer3DistanceToHomeAttckingNet,
         AwayPlayer3DistanceToAwayAttackingNet, AwayPlayer3AngleToHomeAttackingNet,
         AwayPlayer3AngleToAwayAttackingNet, AwayPlayer3AngleToEvent, AwayPlayer3ClosestOpponent,
         AwayPlayer4DistanceToEvent, AwayPlayer4Speed,
         AwayPlayer4SpeedTowardEvent, AwayPlayer4DistanceToHomeAttckingNet,
         AwayPlayer4DistanceToAwayAttackingNet, AwayPlayer4AngleToHomeAttackingNet,
         AwayPlayer4AngleToAwayAttackingNet, AwayPlayer4AngleToEvent, AwayPlayer4ClosestOpponent,
         AwayPlayer5DistanceToEvent, AwayPlayer5Speed,
         AwayPlayer5SpeedTowardEvent, AwayPlayer5DistanceToHomeAttckingNet,
         AwayPlayer5DistanceToAwayAttackingNet, AwayPlayer5AngleToHomeAttackingNet,
         AwayPlayer5AngleToAwayAttackingNet, AwayPlayer5AngleToEvent, AwayPlayer5ClosestOpponent,
         EventTypeWithZone_block_NZ, EventTypeWithZone_block_OZ, EventTypeWithZone_carry_DZ,
         starts_with("EventTypeWithZone"), starts_with("ShotType"),
         SuccessfulEventFlag)

HALOEventsWithTrackingForXGBoost2 <- HALOEventsWithTrackingForXGBoost %>%
  mutate(
    HomePlayersTracked = rowSums(!is.na(cbind(HomePlayer1DistanceToEvent, 
                                              HomePlayer2DistanceToEvent,
                                              HomePlayer3DistanceToEvent, 
                                              HomePlayer4DistanceToEvent,
                                              HomePlayer5DistanceToEvent))),
    AwayPlayersTracked = rowSums(!is.na(cbind(AwayPlayer1DistanceToEvent,
                                              AwayPlayer2DistanceToEvent,
                                              AwayPlayer3DistanceToEvent,
                                              AwayPlayer4DistanceToEvent,
                                              AwayPlayer5DistanceToEvent))),
    HomeTeamSpread = ifelse(
      HomePlayersTracked >= 2,
      apply(cbind(HomePlayer1DistanceToEvent, HomePlayer2DistanceToEvent,
                  HomePlayer3DistanceToEvent, HomePlayer4DistanceToEvent,
                  HomePlayer5DistanceToEvent), 1, sd, na.rm = TRUE), NA_real_
    ),
    AwayTeamSpread = ifelse(
      AwayPlayersTracked >= 2,
      apply(cbind(AwayPlayer1DistanceToEvent, AwayPlayer2DistanceToEvent,
                  AwayPlayer3DistanceToEvent, AwayPlayer4DistanceToEvent,
                  AwayPlayer5DistanceToEvent), 1, sd, na.rm = TRUE), NA_real_
    ),
    HomePlayersWithin10 = rowSums(cbind(HomePlayer1DistanceToEvent, HomePlayer2DistanceToEvent,
                                            HomePlayer3DistanceToEvent, HomePlayer4DistanceToEvent,
                                            HomePlayer5DistanceToEvent) < 10, na.rm = TRUE),
    AwayPlayersWithin10 = rowSums(cbind(AwayPlayer1DistanceToEvent, AwayPlayer2DistanceToEvent,
                                            AwayPlayer3DistanceToEvent, AwayPlayer4DistanceToEvent,
                                            AwayPlayer5DistanceToEvent) < 10, na.rm = TRUE),
    HomePropWithin10 = HomePlayersWithin10 / HomePlayersTracked,
    AwayPropWithin10 = AwayPlayersWithin10 / AwayPlayersTracked,
    NumericalAdvantage = HomePlayersWithin10 - AwayPlayersWithin10,
    HomeTeamAvgSpeed = rowSums(cbind(HomePlayer1Speed, HomePlayer2Speed, HomePlayer3Speed,
                                            HomePlayer4Speed, HomePlayer5Speed), na.rm = TRUE)/HomePlayersTracked,
    AwayTeamAvgSpeed = rowSums(cbind(AwayPlayer1Speed, AwayPlayer2Speed, AwayPlayer3Speed,
                                            AwayPlayer4Speed, AwayPlayer5Speed), na.rm = TRUE)/AwayPlayersTracked,
    HomeTeamAvgSpeedToEvent = rowSums(cbind(HomePlayer1SpeedTowardEvent, HomePlayer2SpeedTowardEvent,
                                            HomePlayer3SpeedTowardEvent, HomePlayer4SpeedTowardEvent,
                                            HomePlayer5SpeedTowardEvent), na.rm = TRUE) /HomePlayersTracked,
    AwayTeamAvgSpeedToEvent = rowSums(cbind(AwayPlayer1SpeedTowardEvent, AwayPlayer2SpeedTowardEvent,
                                            AwayPlayer3SpeedTowardEvent, AwayPlayer4SpeedTowardEvent,
                                            AwayPlayer5SpeedTowardEvent), na.rm = TRUE) /AwayPlayersTracked
  )




HALOEventsWithTrackingForXGBoost12Sec <- HALOEventsWithTrackingForXGBoost2 %>%
  select(-c(TeamGoalWithinNext3Sec, TeamGoalWithinNext5Sec, TeamGoalWithinNext7Sec, TeamGoalWithinNext10Sec, 
            TeamGoalWithinNext11Sec, TeamGoalWithinNext15Sec, TeamGoalWithinNext17Sec, 
            TeamGoalWithinNext20Sec, TeamGoalWithinNext25Sec, TeamGoalWithinNext30Sec)) %>%
  mutate(TeamGoalWithinNext12Sec = ifelse(is.na(TeamGoalWithinNext12Sec), 0, TeamGoalWithinNext12Sec))


HALOEventsWithTrackingForXGBoost_3ClosestPerSide12Sec <- HALOEventsWithTrackingForXGBoost %>%
  select(-c(TeamGoalWithinNext3Sec, TeamGoalWithinNext5Sec, TeamGoalWithinNext7Sec, TeamGoalWithinNext10Sec, 
            TeamGoalWithinNext11Sec, TeamGoalWithinNext15Sec, TeamGoalWithinNext17Sec, 
            TeamGoalWithinNext20Sec, TeamGoalWithinNext25Sec, TeamGoalWithinNext30Sec,
            HomePlayer4DistanceToEvent, HomePlayer4Speed, 
            HomePlayer4SpeedTowardEvent, HomePlayer4DistanceToHomeAttckingNet,
            HomePlayer4DistanceToAwayAttackingNet, HomePlayer4AngleToHomeAttackingNet,
            HomePlayer4AngleToAwayAttackingNet, HomePlayer4AngleToEvent, HomePlayer4ClosestOpponent,
            HomePlayer5DistanceToEvent, HomePlayer5Speed, 
            HomePlayer5SpeedTowardEvent, HomePlayer5DistanceToHomeAttckingNet,
            HomePlayer5DistanceToAwayAttackingNet, HomePlayer5AngleToHomeAttackingNet,
            HomePlayer5AngleToAwayAttackingNet, HomePlayer5AngleToEvent, HomePlayer5ClosestOpponent,
            AwayPlayer4DistanceToEvent, AwayPlayer4Speed,
            AwayPlayer4SpeedTowardEvent, AwayPlayer4DistanceToHomeAttckingNet,
            AwayPlayer4DistanceToAwayAttackingNet, AwayPlayer4AngleToHomeAttackingNet,
            AwayPlayer4AngleToAwayAttackingNet, AwayPlayer4AngleToEvent, AwayPlayer4ClosestOpponent,
            AwayPlayer5DistanceToEvent, AwayPlayer5Speed,
            AwayPlayer5SpeedTowardEvent, AwayPlayer5DistanceToHomeAttckingNet,
            AwayPlayer5DistanceToAwayAttackingNet, AwayPlayer5AngleToHomeAttackingNet,
            AwayPlayer5AngleToAwayAttackingNet, AwayPlayer5AngleToEvent, AwayPlayer5ClosestOpponent
            )) %>%
  mutate(TeamGoalWithinNext12Sec = ifelse(is.na(TeamGoalWithinNext12Sec), 0, TeamGoalWithinNext12Sec),
         HomePlayersTracked = rowSums(!is.na(cbind(HomePlayer1DistanceToEvent, 
                                                   HomePlayer2DistanceToEvent,
                                                   HomePlayer3DistanceToEvent))),
         AwayPlayersTracked = rowSums(!is.na(cbind(AwayPlayer1DistanceToEvent,
                                                   AwayPlayer2DistanceToEvent,
                                                   AwayPlayer3DistanceToEvent))),
         HomeTeamSpread = ifelse(
           HomePlayersTracked >= 2,
           apply(cbind(HomePlayer1DistanceToEvent, HomePlayer2DistanceToEvent,
                       HomePlayer3DistanceToEvent), 1, sd, na.rm = TRUE), NA_real_
         ),
         AwayTeamSpread = ifelse(
           AwayPlayersTracked >= 2,
           apply(cbind(AwayPlayer1DistanceToEvent, AwayPlayer2DistanceToEvent,
                       AwayPlayer3DistanceToEvent), 1, sd, na.rm = TRUE), NA_real_
         ),
         HomePlayersWithin10 = rowSums(cbind(HomePlayer1DistanceToEvent, HomePlayer2DistanceToEvent,
                                             HomePlayer3DistanceToEvent) < 10, na.rm = TRUE),
         AwayPlayersWithin10 = rowSums(cbind(AwayPlayer1DistanceToEvent, AwayPlayer2DistanceToEvent,
                                             AwayPlayer3DistanceToEvent) < 10, na.rm = TRUE),
         HomePropWithin10 = HomePlayersWithin10 / HomePlayersTracked,
         AwayPropWithin10 = AwayPlayersWithin10 / AwayPlayersTracked,
         NumericalAdvantage = HomePlayersWithin10 - AwayPlayersWithin10,
         HomeTeamAvgSpeed = rowSums(cbind(HomePlayer1Speed, HomePlayer2Speed, HomePlayer3Speed), na.rm = TRUE)/HomePlayersTracked,
         AwayTeamAvgSpeed = rowSums(cbind(AwayPlayer1Speed, AwayPlayer2Speed, AwayPlayer3Speed), na.rm = TRUE)/AwayPlayersTracked,
         HomeTeamAvgSpeedToEvent = rowSums(cbind(HomePlayer1SpeedTowardEvent, HomePlayer2SpeedTowardEvent,
                                                 HomePlayer3SpeedTowardEvent), na.rm = TRUE) /HomePlayersTracked,
         AwayTeamAvgSpeedToEvent = rowSums(cbind(AwayPlayer1SpeedTowardEvent, AwayPlayer2SpeedTowardEvent,
                                                 AwayPlayer3SpeedTowardEvent), na.rm = TRUE) /AwayPlayersTracked,
         HomeTeamAvgDistToHomeAttackingNet = rowSums(cbind(HomePlayer1DistanceToHomeAttckingNet, HomePlayer2DistanceToHomeAttckingNet, HomePlayer3DistanceToHomeAttckingNet), na.rm = TRUE)/HomePlayersTracked,
         AwayTeamAvgDistToHomeAttackingNet = rowSums(cbind(AwayPlayer1DistanceToHomeAttckingNet, AwayPlayer2DistanceToHomeAttckingNet, AwayPlayer3DistanceToHomeAttckingNet), na.rm = TRUE)/AwayPlayersTracked,
         HomeTeamAvgDistToAwayAttackingNet = rowSums(cbind(HomePlayer1DistanceToAwayAttackingNet, HomePlayer2DistanceToAwayAttackingNet, HomePlayer3DistanceToAwayAttackingNet), na.rm = TRUE)/HomePlayersTracked,
         AwayTeamAvgDistToAwayAttackingNet = rowSums(cbind(AwayPlayer1DistanceToAwayAttackingNet, AwayPlayer2DistanceToAwayAttackingNet, AwayPlayer3DistanceToAwayAttackingNet), na.rm = TRUE)/AwayPlayersTracked,
         ClosestDefenderToEvent = pmin(AwayPlayer1DistanceToEvent, AwayPlayer2DistanceToEvent,
                                       AwayPlayer3DistanceToEvent, na.rm = TRUE),
         ClosestHomePlayerToEvent = pmin(HomePlayer1DistanceToEvent, HomePlayer2DistanceToEvent,
                                         HomePlayer3DistanceToEvent, na.rm = TRUE),
         SpaceAtEvent = ClosestDefenderToEvent / (ClosestHomePlayerToEvent + 0.001)
         )

