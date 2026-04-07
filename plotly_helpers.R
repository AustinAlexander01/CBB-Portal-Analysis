
# ==============================================================================
# PLOTLY_HELPERS.R — FULL FILE (Season timeline plotly added; name match fixed)
# Place this entire file at /srv/connect/apps/radar_app/plotly_helpers.R (or your local)
# Ensure it is sourced BEFORE server uses build_season_shift_timeline_* functions.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(plotly)
  library(scales)
  library(glue)
  library(memoise)
})

# ------------------------- small utilities ------------------------------------
nzchar0 <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  nzchar(x)
}

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a)) && nzchar0(a[1])) a else b
}

# ------------------------- name normalization ---------------------------------

# MODIFIED: Enhanced event palette with assist color for dark mode visibility
get_event_palette <- function(dark_mode) {
  if (isTRUE(dark_mode)) {
    # MODIFIED: Bright readable colors for dark background
    list(
      shot_make = "#10b981",   # bright emerald green
      shot_miss = "#f87171",   # bright coral red/pink 
      block     = "#60a5fa",   # bright sky blue
      assist    = "#fbbf24"    # bright amber/yellow
    )
  } else {
    list(
      shot_make = "black",
      shot_miss = "black",
      block     = "black",
      assist    = "black"
    )
  }
}
norm_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x %>%
    stringr::str_replace_all("\\.", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim() %>%
    stringr::str_to_title()
}

base_name <- function(x) {
  x <- norm_name(x)
  x %>%
    stringr::str_remove_all("[^A-Za-z ]") %>%
    stringr::str_squish()
}

# strict key for matching across sources (vector-safe)
norm_name_strict <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x %>%
    str_replace_all("\\.", " ") %>%
    str_replace_all(",", " ") %>%
    str_replace_all("'", "") %>%
    str_replace_all("-", " ") %>%
    str_to_lower() %>%
    str_squish() %>%
    str_remove("\\s+(jr|sr|ii|iii|iv|v)\\b$") %>%
    str_squish()
}

clock_to_sec <- function(x) {
  x0 <- as.character(x)
  x0[is.na(x0)] <- ""
  
  # already numeric-like
  num <- suppressWarnings(as.numeric(x0))
  ok_num <- is.finite(num)
  
  out <- rep(NA_real_, length(x0))
  out[ok_num] <- num[ok_num]
  
  # parse "MM:SS"
  idx <- !ok_num & nzchar(x0) & stringr::str_detect(x0, "^\\s*\\d+\\s*:\\s*\\d{2}\\s*$")
  if (any(idx)) {
    mm <- suppressWarnings(as.numeric(stringr::str_match(x0[idx], "^\\s*(\\d+)\\s*:")[,2]))
    ss <- suppressWarnings(as.numeric(stringr::str_match(x0[idx], ":\\s*(\\d{2})\\s*$")[,2]))
    out[idx] <- 60 * mm + ss
  }
  
  out
}


name_keys <- function(x) {
  full <- norm_name_strict(x)
  first <- stringr::word(full, 1)
  last  <- stringr::word(full, -1)
  
  first[!nzchar0(first)] <- NA_character_
  last[!nzchar0(last)]   <- NA_character_
  
  tibble(
    key_full       = full,
    key_first_last = ifelse(!is.na(first) & !is.na(last), paste(first, last), NA_character_),
    key_fi_last    = ifelse(!is.na(first) & !is.na(last), paste0(substr(first, 1, 1), " ", last), NA_character_),
    key_last_fi    = ifelse(!is.na(first) & !is.na(last), paste0(last, " ", substr(first, 1, 1)), NA_character_)
  )
}


# 
name_key <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  stringr::str_replace_all(tolower(x), "[^a-z]", "")
}


norm_player_key <- function(x) {
  # Normalizes for joining/matching: "John Doe" -> "JOHN DOE"
  x %>%
    as.character() %>%
    str_replace_all("\\.", " ") %>%
    str_replace_all("[^A-Za-z\\s]", " ") %>%
    str_squish() %>%
    str_to_upper()
}

ensure_cols <- function(df, cols, fill = NA) {
  for (cc in cols) if (!cc %in% names(df)) df[[cc]] <- fill
  df
}

# Pull boxscores for the timeline game set and return per-game stats for the selected player
get_player_boxscore_by_game <- function(game_ids, team_name, player_raw, verbose = TRUE) {
  game_ids <- unique(na.omit(as.character(game_ids)))
  if (length(game_ids) == 0) return(tibble())
  
  # bigballR::get_box_scores() is the package function intended to pull boxscores. :contentReference[oaicite:1]{index=1}
  bs <- try(bigballR::get_box_scores(game_ids), silent = TRUE)
  if (inherits(bs, "try-error") || is.null(bs) || !is.data.frame(bs) || nrow(bs) == 0) {
    if (isTRUE(verbose)) message("[timeline] boxscores unavailable from get_box_scores(); returning empty.")
    return(tibble())
  }
  
  # Harden expected columns; actual names can vary slightly across bigballR versions / NCAA pages
  # We’ll try a few common candidates for player/team columns.
  cand_player_cols <- c("Player", "PLAYER", "player", "Name", "NAME", "Athlete")
  cand_team_cols   <- c("Team", "TEAM", "team", "School", "SCHOOL")
  
  player_col <- cand_player_cols[cand_player_cols %in% names(bs)][1]
  team_col   <- cand_team_cols[cand_team_cols %in% names(bs)][1]
  
  if (is.na(player_col) || is.na(team_col)) {
    if (isTRUE(verbose)) message("[timeline] boxscore df missing recognizable Player/Team columns; returning empty.")
    return(tibble())
  }
  
  # Standard stat columns we want
  stat_cols <- c("PTS","REB","AST","TOV","STL","BLK")
  
  bs2 <- bs %>%
    mutate(
      game_id     = if ("game_id" %in% names(.)) as.character(.data$game_id)
      else if ("Game_ID" %in% names(.)) as.character(.data$Game_ID)
      else as.character(NA),
      team_key    = norm_player_key(.data[[team_col]]),
      player_key  = norm_player_key(.data[[player_col]])
    ) %>%
    filter(!is.na(.data$game_id))
  
  target_team_key   <- norm_player_key(team_name)
  target_player_key <- norm_player_key(player_raw)
  
  out <- bs2 %>%
    filter(.data$team_key == target_team_key) %>%
    filter(.data$player_key == target_player_key) %>%
    select(game_id, any_of(stat_cols)) %>%
    distinct(game_id, .keep_all = TRUE)
  
  # If strict match fails (name formatting differences), fall back to "contains last name"
  if (nrow(out) == 0) {
    last <- str_split(target_player_key, "\\s+")[[1]]
    last <- if (length(last) >= 1) last[length(last)] else target_player_key
    if (nzchar(last)) {
      out <- bs2 %>%
        filter(.data$team_key == target_team_key) %>%
        filter(str_detect(.data$player_key, fixed(last))) %>%
        select(game_id, any_of(stat_cols)) %>%
        distinct(game_id, .keep_all = TRUE)
    }
  }
  
  out
}

filter_shift_rows_for_player <- function(shift_df, player_raw) {
  if (is.null(shift_df) || !is.data.frame(shift_df) || nrow(shift_df) == 0) return(shift_df[0, , drop = FALSE])
  if (is.null(player_raw) || !nzchar(as.character(player_raw))) return(shift_df[0, , drop = FALSE])
  
  # Identify which column contains the player's name in shift rows
  name_col <- NULL
  if ("CleanName" %in% names(shift_df)) {
    name_col <- "CleanName"
  } else if ("Player" %in% names(shift_df)) {
    name_col <- "Player"
  } else if ("player" %in% names(shift_df)) {
    name_col <- "player"
  } else if ("Name" %in% names(shift_df)) {
    name_col <- "Name"
  } else {
    # no recognizable name column
    return(shift_df[0, , drop = FALSE])
  }
  
  sel_norm <- norm_name(player_raw)
  sel_base <- base_name(player_raw)
  
  shift_df %>%
    dplyr::mutate(
      .nm_norm = norm_name(.data[[name_col]]),
      .nm_base = base_name(.data[[name_col]])
    ) %>%
    dplyr::filter(.data$.nm_norm == sel_norm | .data$.nm_base == sel_base) %>%
    dplyr::select(-dplyr::any_of(c(".nm_norm", ".nm_base")))
}

filter_shift_stints_for_combo <- function(shifts, combo_players, team_name) {
  stopifnot(is.data.frame(shifts))
  combo_players <- as.character(combo_players)
  combo_players <- combo_players[!is.na(combo_players) & nzchar(trimws(combo_players))]
  combo_players <- standardize_player_code(combo_players)
  
  if (!(length(combo_players) %in% c(2, 5))) return(shifts)
  if (!nzchar(team_name)) return(shifts)
  
  need <- c("Home", "Away")
  miss <- setdiff(need, names(shifts))
  if (length(miss) > 0) stop("filter_shift_stints_for_combo: shifts missing: ", paste(miss, collapse = ", "))
  
  # lineup columns
  home_cols <- paste0("Home.", 1:5)
  away_cols <- paste0("Away.", 1:5)
  has_home  <- all(home_cols %in% names(shifts))
  has_away  <- all(away_cols %in% names(shifts))
  if (!has_home || !has_away) {
    stop("filter_shift_stints_for_combo: shifts missing lineup cols Home.1..Home.5 and/or Away.1..Away.5")
  }
  
  team_name_std <- trimws(team_name)
  
  # determine team side per row
  side <- dplyr::case_when(
    trimws(shifts$Home) == team_name_std ~ "home",
    trimws(shifts$Away) == team_name_std ~ "away",
    TRUE ~ NA_character_
  )
  
  # compute has_all safely (no tidyselect pronouns, no ..data)
  has_all <- vapply(seq_len(nrow(shifts)), function(i) {
    if (is.na(side[i])) return(FALSE)
    
    lineup <- if (identical(side[i], "home")) {
      unname(unlist(shifts[i, home_cols], use.names = FALSE))
    } else {
      unname(unlist(shifts[i, away_cols], use.names = FALSE))
    }
    
    lineup <- standardize_player_code(lineup)
    lineup <- lineup[!is.na(lineup) & nzchar(trimws(lineup))]
    
    all(combo_players %in% lineup)
  }, logical(1))
  
  out <- shifts[has_all, , drop = FALSE]
  out
}


build_target_key_set_key <- function(player_raw) {
  keys <- name_key(player_raw)
  
  if (exists("alias_map", inherits = TRUE) && length(alias_map) > 0) {
    amap_from <- name_key(names(alias_map))
    amap_to   <- name_key(unname(alias_map))
    
    hit_from <- which(amap_from == keys)
    if (length(hit_from) > 0) keys <- c(keys, amap_to[hit_from])
    
    hit_to <- which(amap_to == keys)
    if (length(hit_to) > 0) keys <- c(keys, amap_from[hit_to])
  }
  
  unique(keys[nzchar(keys)])
}

get_team_player_names_from_game_lineups <- function(pbp, team_name) {
  if (is.null(pbp) || !is.data.frame(pbp) || nrow(pbp) == 0) return(character(0))
  
  # Prefer your existing lineup builder, since it already assigns Team and P1..P5
  lns <- tryCatch(suppressMessages(get_lineups_fast_acc(pbp)), error = function(e) NULL)
  if (is.null(lns) || !is.data.frame(lns) || nrow(lns) == 0) return(character(0))
  if (!all(c("Team", "P1","P2","P3","P4","P5") %in% names(lns))) return(character(0))
  
  lns %>%
    dplyr::filter(str_trim(.data$Team) == str_trim(team_name)) %>%
    dplyr::select(dplyr::all_of(c("P1","P2","P3","P4","P5"))) %>%
    unlist(use.names = FALSE) %>%
    as.character() %>%
    str_trim() %>%
    (\(v) v[!is.na(v) & nzchar(v) & str_detect(v, "[A-Za-z]")])() %>%
    unique()
}

normalize_event_context_cols <- function(events_all) {
  if (is.null(events_all) || !is.data.frame(events_all) || nrow(events_all) == 0) return(events_all)
  
  prefer_y <- function(base) {
    y <- paste0(base, ".y")
    x <- paste0(base, ".x")
    if (y %in% names(events_all) && x %in% names(events_all)) {
      events_all[[base]] <<- dplyr::coalesce(events_all[[y]], events_all[[x]])
      events_all[[x]] <<- NULL
      events_all[[y]] <<- NULL
    } else if (y %in% names(events_all) && !(base %in% names(events_all))) {
      names(events_all)[names(events_all) == y] <<- base
    } else if (x %in% names(events_all) && !(base %in% names(events_all))) {
      names(events_all)[names(events_all) == x] <<- base
    }
    invisible(NULL)
  }
  
  bases <- c(
    "Home","Away","Home_Score","Away_Score",
    "Home.1","Home.2","Home.3","Home.4","Home.5",
    "Away.1","Away.2","Away.3","Away.4","Away.5"
  )
  
  for (b in bases) prefer_y(b)
  
  # drop any lingering suffix cols (defensive)
  drop_cols <- grep("\\.(x|y)$", names(events_all), value = TRUE)
  if (length(drop_cols) > 0) events_all <- dplyr::select(events_all, -dplyr::all_of(drop_cols))
  
  events_all
}


build_selected_player_events_one_game <- function(pbp, team_name, player_raw, verbose = FALSE) {
  if (is.null(pbp) || !is.data.frame(pbp) || nrow(pbp) == 0) return(tibble::tibble())
  
  nzchar0 <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
  
  # allow flexible gid col (matches your other code)
  gid_col <- intersect(c("ID", "Game_ID", "game_id", "GameId", "gameId"), names(pbp))[1]
  if (is.na(gid_col) || !nzchar(as.character(gid_col))) return(tibble::tibble())
  
  need_cols <- c(
    "Game_Seconds","Event_Team","Event_Type","Event_Result","Event_Description",
    "Player_1","Player_2","Shot_Value","Time","Half_Status"
  )
  if (!all(need_cols %in% names(pbp))) return(tibble::tibble())
  
  # optional context cols (don’t require; fill NA if missing)
  ctx_cols <- c(
    "Home","Away","Home_Score","Away_Score",
    "Home.1","Home.2","Home.3","Home.4","Home.5",
    "Away.1","Away.2","Away.3","Away.4","Away.5"
  )
  for (cc in ctx_cols) if (!cc %in% names(pbp)) pbp[[cc]] <- NA
  
  sig <- player_signature(player_raw)
  target_keys <- sig$keys
  
  key_hit <- function(x) {
    kt <- cand_keys_tbl(x)
    (kt$full %in% target_keys) |
      (kt$first_last %in% target_keys) |
      (kt$fi_last %in% target_keys) |
      (kt$last_fi %in% target_keys)
  }
  
  pbp2 <- pbp %>%
    dplyr::mutate(
      pbp_row_id  = dplyr::row_number(),
      game_id     = as.character(.data[[gid_col]]),
      Game_Seconds= suppressWarnings(as.numeric(.data$Game_Seconds)),
      is_team     = stringr::str_trim(tolower(.data$Event_Team)) == stringr::str_trim(tolower(team_name)),
      Event_Description = as.character(.data$Event_Description),
      Player_1_raw = as.character(.data$Player_1),
      Player_2_raw = as.character(.data$Player_2),
      
      Player_2_from_desc = stringr::str_match(
        dplyr::coalesce(.data$Event_Description, ""),
        "\\-\\s*([^,]+),\\s*assist\\b"
      )[, 2],
      
      Player_2_filled = dplyr::if_else(
        !nzchar0(.data$Player_2_raw) &
          stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(.data$Event_Description, "")), "\\bassist\\b") &
          nzchar0(.data$Player_2_from_desc),
        .data$Player_2_from_desc,
        .data$Player_2_raw
      ),
      
      Player_1 = .data$Player_1_raw,
      Player_2 = .data$Player_2_filled,
      
      et = tolower(as.character(.data$Event_Type)),
      made = dplyr::case_when(
        tolower(as.character(.data$Event_Result)) == "made"   ~ TRUE,
        tolower(as.character(.data$Event_Result)) == "missed" ~ FALSE,
        TRUE ~ NA
      ),
      
      shot_value = suppressWarnings(as.numeric(.data$Shot_Value)),
      shot_type = dplyr::case_when(
        !is.na(.data$shot_value) & .data$shot_value == 1 ~ "FT",
        !is.na(.data$shot_value) & .data$shot_value == 2 ~ "2PT",
        !is.na(.data$shot_value) & .data$shot_value == 3 ~ "3PT",
        TRUE ~ NA_character_
      ),
      is_shot = !is.na(.data$shot_type) & !is.na(.data$made),
      
      actor_hit    = key_hit(.data$Player_1),
      assister_hit = key_hit(.data$Player_2),
      
      is_assist_for_selected = .data$is_shot & dplyr::coalesce(.data$made, FALSE) & .data$assister_hit,
      
      is_turnover = !is.na(.data$et) & .data$et %in% c("turnover"),
      is_steal    = !is.na(.data$et) & .data$et %in% c("steal"),
      is_block    = !is.na(.data$et) & .data$et %in% c("blocked shot","block","blockedshot"),
      is_rebound  = !is.na(.data$et) & stringr::str_detect(.data$et, "rebound")
    ) %>%
    dplyr::filter(.data$is_team) %>%
    dplyr::filter(is.finite(.data$Game_Seconds), .data$Game_Seconds >= 0) %>%
    dplyr::filter(.data$actor_hit | .data$assister_hit)
  
  if (isTRUE(verbose)) {
    message(
      "[events one game] rows=", nrow(pbp2),
      " | actor_hit=", sum(pbp2$actor_hit %in% TRUE, na.rm = TRUE),
      " | assister_hit=", sum(pbp2$assister_hit %in% TRUE, na.rm = TRUE),
      " | assists_for_selected=", sum(pbp2$is_assist_for_selected %in% TRUE, na.rm = TRUE)
    )
  }
  
  out <- pbp2 %>%
    dplyr::transmute(
      game_id      = as.character(.data$game_id),
      pbp_row_id   = suppressWarnings(as.integer(.data$pbp_row_id)),
      
      Game_Seconds = .data$Game_Seconds,
      Half_Status  = as.character(.data$Half_Status),
      Time         = as.character(.data$Time),
      Event_Description = as.character(.data$Event_Description),
      
      Event_Type   = as.character(.data$Event_Type),
      Event_Result = as.character(.data$Event_Result),
      Event_Team   = as.character(.data$Event_Team),
      
      Shot_Value   = .data$shot_value,
      made         = .data$made,
      Player_1     = as.character(.data$Player_1),
      Player_2     = as.character(.data$Player_2),
      
      # context you want for hover + future lineup filtering
      Home       = as.character(.data$Home),
      Away       = as.character(.data$Away),
      Home_Score = suppressWarnings(as.numeric(.data$Home_Score)),
      Away_Score = suppressWarnings(as.numeric(.data$Away_Score)),
      
      Home.1 = as.character(.data$Home.1),
      Home.2 = as.character(.data$Home.2),
      Home.3 = as.character(.data$Home.3),
      Home.4 = as.character(.data$Home.4),
      Home.5 = as.character(.data$Home.5),
      
      Away.1 = as.character(.data$Away.1),
      Away.2 = as.character(.data$Away.2),
      Away.3 = as.character(.data$Away.3),
      Away.4 = as.character(.data$Away.4),
      Away.5 = as.character(.data$Away.5),
      
      Player = dplyr::case_when(
        .data$is_assist_for_selected ~ as.character(.data$Player_2),
        .data$is_shot & .data$actor_hit ~ as.character(.data$Player_1),
        TRUE ~ as.character(.data$Player_1)
      ),
      
      Event = dplyr::case_when(
        .data$is_assist_for_selected            ~ "Assist",
        .data$is_shot & .data$actor_hit         ~ .data$shot_type,
        .data$is_turnover & .data$actor_hit     ~ "Turnover",
        .data$is_steal    & .data$actor_hit     ~ "Steal",
        .data$is_block    & .data$actor_hit     ~ "Block",
        .data$is_rebound  & .data$actor_hit     ~ "Rebound",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(.data$Event)) %>%
    dplyr::mutate(Player_hit = key_hit(.data$Player)) %>%
    dplyr::filter(.data$Player_hit) %>%
    dplyr::select(-.data$Player_hit)
  
  out
}


# PBP-style normalizer:
# "DJ.WAGNER" -> "dj wagner"
# "D.J. Wagner" -> "dj wagner"
# "D J WAGNER" -> "dj wagner"
norm_name_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x %>%
    str_replace_all("[\\._]", " ") %>%     # dots/underscores to spaces
    str_replace_all("[,']", "") %>%        # drop commas/apostrophes
    str_replace_all("-", " ") %>%
    str_to_lower() %>%
    str_squish() %>%
    str_remove("\\s+(jr|sr|ii|iii|iv|v)\\b$") %>%
    str_squish()
}

name_parts <- function(x) {
  x2 <- norm_name_key(x)
  if (!nzchar0(x2)) return(list(full = "", first = "", last = "", fi = ""))
  toks <- unlist(str_split(x2, "\\s+"))
  toks <- toks[nzchar0(toks)]
  if (length(toks) == 0) return(list(full = "", first = "", last = "", fi = ""))
  first <- toks[1]
  last  <- toks[length(toks)]
  list(
    full  = x2,
    first = first,
    last  = last,
    fi    = substr(first, 1, 1)
  )
}

# Build a robust target “signature” for a selected player
player_signature <- function(player_raw) {
  p <- name_parts(player_raw)
  # keys we’ll match against
  keys <- unique(c(
    p$full,
    paste0(p$first, " ", p$last),
    paste0(p$fi, " ", p$last),
    paste0(p$last, " ", p$fi)
  ))
  keys <- keys[nzchar0(keys)]
  list(
    full = p$full,
    first = p$first,
    last = p$last,
    fi = p$fi,
    keys = keys
  )
}

# Candidate keys for a vector of names (vectorized)
cand_keys_tbl <- function(x) {
  x_full <- norm_name_key(x)
  parts  <- lapply(x, name_parts)
  first  <- vapply(parts, `[[`, "", "first")
  last   <- vapply(parts, `[[`, "", "last")
  fi     <- vapply(parts, `[[`, "", "fi")
  
  tibble(
    full       = x_full,
    first_last = ifelse(nzchar0(first) & nzchar0(last), paste0(first, " ", last), ""),
    fi_last    = ifelse(nzchar0(fi) & nzchar0(last),   paste0(fi, " ", last), ""),
    last_fi    = ifelse(nzchar0(fi) & nzchar0(last),   paste0(last, " ", fi), ""),
    last_only  = last
  )
}

# ---- SHIFT matcher with last-name fallback (only if unique) -------------------
match_player_in_shift_data_key <- function(sd, player_raw, pbp = NULL, team_name = NULL, verbose = FALSE) {
  if (is.null(sd) || !is.data.frame(sd) || nrow(sd) == 0) return(sd[0, , drop = FALSE])
  if (!"CleanName" %in% names(sd)) return(sd[0, , drop = FALSE])
  
  targets <- build_target_key_set_key(player_raw)
  if (length(targets) == 0) return(sd[0, , drop = FALSE])
  
  # Ensure sd has a stable key column (do NOT rely on spaces/Title Case)
  if (!"CleanKey" %in% names(sd)) {
    sd <- sd %>% dplyr::mutate(CleanKey = name_key(.data$CleanName))
  }
  
  
  
  out <- sd %>% dplyr::filter(.data$CleanKey %in% targets)
  if (nrow(out) > 0) return(out)
  
  # If miss: try “roster candidates” from lineups to account for source drift
  if (!is.null(pbp) && !is.null(team_name)) {
    roster_names <- get_team_player_names_from_game_lineups(pbp, team_name)
    roster_keys  <- name_key(roster_names)
    
    # If UI selection matches some roster key, treat that as authoritative
    if (any(roster_keys %in% targets)) {
      hit_key <- roster_keys[which(roster_keys %in% targets)[1]]
      out2 <- sd %>% dplyr::filter(.data$CleanKey == hit_key)
      if (nrow(out2) > 0) return(out2)
    }
  }
  
  if (isTRUE(verbose)) {
    msg <- paste0(
      "[shift match miss] player_raw=", player_raw,
      " | targets=", paste(targets, collapse = ", "),
      " | sd keys sample=", paste(utils::head(unique(sd$CleanKey), 10), collapse = ", ")
    )
    message(msg)
  }
  
  sd[0, , drop = FALSE]
}

# ---- BOX matcher with last-name fallback (only if unique) -------------------
match_player_in_box_key <- function(ps, team_name, player_raw) {
  if (is.null(ps) || !is.data.frame(ps) || nrow(ps) == 0) return(ps[0, , drop = FALSE])
  if (!all(c("Team", "Player") %in% names(ps))) return(ps[0, , drop = FALSE])
  
  targets <- build_target_key_set_key(player_raw)
  if (length(targets) == 0) return(ps[0, , drop = FALSE])
  
  ps_team <- ps %>%
    dplyr::filter(stringr::str_trim(stringr::str_to_lower(.data$Team)) ==
                    stringr::str_trim(stringr::str_to_lower(team_name))) %>%
    dplyr::mutate(Player_key = name_key(.data$Player))
  
  ps_team %>%
    dplyr::filter(.data$Player_key %in% targets) %>%
    dplyr::slice(1)
}

build_target_key_set <- function(player_raw) {
  alias_targets <- character(0)
  
  if (exists("alias_map", inherits = TRUE) && length(alias_map) > 0) {
    amap <- stats::setNames(norm_name_strict(unname(alias_map)), norm_name_strict(names(alias_map)))
    sel_norm <- norm_name_strict(player_raw)
    
    if (sel_norm %in% names(amap))  alias_targets <- c(alias_targets, amap[[sel_norm]])
    if (sel_norm %in% unname(amap)) alias_targets <- c(alias_targets, names(amap)[unname(amap) == sel_norm])
  }
  
  all_names <- unique(c(player_raw, alias_targets))
  kt <- purrr::map_dfr(all_names, name_keys)
  
  message("target key set")
  print(kt)
  
  unique(na.omit(c(
    kt$key_full,
    kt$key_first_last,
    kt$key_fi_last,
    kt$key_last_fi
  )))
}

match_player_in_shift_data <- function(sd, player_raw, verbose = FALSE) {
  if (is.null(sd) || !is.data.frame(sd) || nrow(sd) == 0) return(sd[0, , drop = FALSE])
  if (!"CleanName" %in% names(sd)) return(sd[0, , drop = FALSE])
  
  target_keys <- build_target_key_set(player_raw)
  if (length(target_keys) == 0) return(sd[0, , drop = FALSE])
  
  ck <- name_keys(sd$CleanName)
  hits <- (ck$key_full       %in% target_keys) |
    (ck$key_first_last %in% target_keys) |
    (ck$key_fi_last    %in% target_keys) |
    (ck$key_last_fi    %in% target_keys)
  
  if (isTRUE(verbose)) {
    message(
      "[match] player_raw=", player_raw,
      " | targets=", length(target_keys),
      " | sd rows=", nrow(sd),
      " | hits=", sum(hits, na.rm = TRUE),
      " | CleanName sample=", paste(head(unique(sd$CleanName), 8), collapse = " | ")
    )
  }
  
  sd[hits, , drop = FALSE]
}

# ------------------------- date coercion --------------------------------------
coerce_to_date <- function(x, tz = "America/New_York") {
  if (inherits(x, "Date")) return(x)
  
  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(as.Date(x, tz = tz))
  }
  
  if (is.numeric(x)) {
    out <- suppressWarnings(as.Date(x, origin = "1899-12-30"))
    bad <- is.na(out) | out > as.Date("2100-01-01") | out < as.Date("1900-01-01")
    if (any(bad, na.rm = TRUE)) {
      out2 <- suppressWarnings(as.Date(as.POSIXct(x, origin = "1970-01-01", tz = tz)))
      out[bad] <- out2[bad]
    }
    return(out)
  }
  
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- ""
  
  x_chr2 <- stringr::str_trim(stringr::str_replace(x_chr, "T.*$", ""))
  x_chr2 <- stringr::str_trim(stringr::str_replace(x_chr2, "\\s+\\d{1,2}:\\d{2}(:\\d{2})?\\s*(AM|PM)?$", ""))
  
  fmts <- c("%Y-%m-%d","%m/%d/%Y","%m/%d/%y","%b %d, %Y","%B %d, %Y","%d-%b-%Y","%Y/%m/%d")
  out <- rep(as.Date(NA), length(x_chr2))
  
  for (f in fmts) {
    idx <- is.na(out) & nzchar0(x_chr2)
    if (!any(idx)) break
    tmp <- suppressWarnings(as.Date(x_chr2[idx], format = f))
    out[idx] <- tmp
  }
  
  idx <- is.na(out) & nzchar0(x_chr2)
  if (any(idx)) out[idx] <- suppressWarnings(as.Date(x_chr2[idx]))
  
  out
}

# ------------------------- colors / labels ------------------------------------
# ------------------------- colors / labels ------------------------------------
# Strict sign-based diverging scale:
#   pm < 0  -> shades of red
#   pm = 0  -> lightgrey
#   pm > 0  -> shades of green
# Fixed domain [-10, 10], clamped
pm_to_color <- function(pm, pm_min = -10, pm_max = 10, eps = 1e-9) {
  pm <- suppressWarnings(as.numeric(pm))
  out <- rep(NA_character_, length(pm))
  
  ok <- is.finite(pm)
  if (!any(ok)) return(out)
  
  v <- pm[ok]
  v <- pmax(pmin(v, pm_max), pm_min)
  
  is0 <- abs(v) <= eps
  neg <- v < -eps
  pos <- v >  eps
  
  out_ok <- character(length(v))
  out_ok[is0] <- "lightgrey"
  
  # negative side: pm_min .. 0  -> red -> white
  if (any(neg)) {
    neg_pal <- scales::col_numeric(
      palette = c("red", "white"),
      domain  = c(pm_min, 0)
    )
    out_ok[neg] <- neg_pal(v[neg])
  }
  
  # positive side: 0 .. pm_max -> white -> green
  if (any(pos)) {
    pos_pal <- scales::col_numeric(
      palette = c("white", "green"),
      domain  = c(0, pm_max)
    )
    out_ok[pos] <- pos_pal(v[pos])
  }
  
  out[ok] <- out_ok
  out
}


pm_to_hex <- function(pm, pm_min = -10, pm_max = 10, zero_grey = TRUE) {
  pm_num <- suppressWarnings(as.numeric(pm))
  ifelse(
    zero_grey & !is.na(pm_num) & pm_num == 0,
    "#D3D3D3",
    {
      pm_clamped <- pmax(pmin(pm_num, pm_max), pm_min)
      t <- (pm_clamped - pm_min) / (pm_max - pm_min)
      r <- ifelse(t <= 0.5, 231 + (255 - 231) * (t / 0.5), 255 + ( 39 - 255) * ((t - 0.5) / 0.5))
      g <- ifelse(t <= 0.5,  76 + (255 -  76) * (t / 0.5), 255 + (174 - 255) * ((t - 0.5) / 0.5))
      b <- ifelse(t <= 0.5,  60 + (255 -  60) * (t / 0.5), 255 + ( 96 - 255) * ((t - 0.5) / 0.5))
      grDevices::rgb(pmax(pmin(r,255),0), pmax(pmin(g,255),0), pmax(pmin(b,255),0), maxColorValue = 255)
    }
  )
}

sec_to_clock <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- NA_real_
  mm <- floor(x / 60)
  ss <- round(x %% 60)
  sprintf("%d:%02d", mm, ss)
}

fmt_stat <- function(x) {
  xi <- suppressWarnings(as.integer(round(as.numeric(x))))
  ifelse(is.na(xi), "NA", as.character(xi))
}

make_stats_line <- function(PTS, REB, AST, TOV, STL, BLK) {
  paste0(
    fmt_stat(PTS), "p · ",
    fmt_stat(REB), "r · ",
    fmt_stat(AST), "a · ",
    fmt_stat(TOV), "to · ",
    fmt_stat(STL), "stl · ",
    fmt_stat(BLK), "blk"
  )
}

make_game_strip_label <- function(game_label, stats_line, game_id) {
  paste0(
    game_label,
    "<br><span style='font-size:9px; color:#666;'>", stats_line, "</span>",
    "  (", game_id, ")"
  )
}

# ------------------------- hover helpers --------------------------------------
make_hover_shift <- function(player_raw, team_name, game_label, game_date, stats_line, start, end, pm) {
  pm_txt <- ifelse(is.na(pm), "NA", sprintf("%+g", pm))
  paste0(
    "<b>", player_raw, "</b>",
    "<br>", team_name, " — ", game_label,
    if (!is.na(game_date)) paste0(" (", as.character(game_date), ")") else "",
    "<br>", stats_line,
    "<br><br><b>Shift</b>: ", sec_to_clock(start), " – ", sec_to_clock(end),
    " (", sec_to_clock(end - start), ")",
    "<br><b>+/-</b>: ", pm_txt
  )
}

make_hover_event <- function(player_raw, team_name, game_label, stats_line, event, made, shot_value, time_txt, desc) {
  made_txt <- if (!is.na(made)) ifelse(isTRUE(made), "Make", "Miss") else ""
  sv_txt   <- if (!is.na(shot_value)) paste0(" · ", shot_value, "pt") else ""
  extra    <- paste0(event, if (nzchar0(made_txt)) paste0(" (", made_txt, ")") else "", sv_txt)
  desc2    <- as.character(desc %||% "")
  desc2[is.na(desc2)] <- ""
  
  paste0(
    "<b>", player_raw, "</b>",
    "<br>", team_name, " — ", game_label,
    "<br>", stats_line,
    "<br><br><b>", extra, "</b>",
    if (nzchar0(time_txt)) paste0("<br>", time_txt) else "",
    if (nzchar0(desc2)) paste0("<br>", desc2) else ""
  )
}

# ------------------------- row mapping ----------------------------------------
build_row_map <- function(shift_data) {
  stopifnot(is.data.frame(shift_data))
  if (!"label" %in% names(shift_data)) shift_data$label <- shift_data$CleanName
  lvl <- shift_data %>% dplyr::distinct(.data$label) %>% dplyr::pull(.data$label) %>% as.character()
  tibble(label = lvl, row = seq_along(lvl))
}

# ==============================================================================
# INDIVIDUAL GAME PLOTLY (existing pattern; keep compatible)
# ==============================================================================
build_shift_plotly <- function(shift_data, pbp, team_name, final_score,
                               dark_mode = FALSE,
                               pm_min = -10, pm_max = 10) {
  
  stopifnot(is.data.frame(shift_data), is.data.frame(pbp))
  if (!"label" %in% names(shift_data)) shift_data$label <- shift_data$CleanName
  
  row_map <- build_row_map(shift_data)
  
  seg <- shift_data %>%
    dplyr::left_join(row_map, by = "label") %>%
    dplyr::mutate(
      y   = .data$row,
      col = pm_to_color(.data$team_pm, pm_min, pm_max)
    )
  
  halftime     <- 1200
  final_second <- suppressWarnings(max(shift_data$end, na.rm = TRUE))
  max_row      <- suppressWarnings(max(row_map$row, na.rm = TRUE))
  
  pbp_events <- pbp %>%
    dplyr::mutate(
      is_team    = stringr::str_trim(tolower(.data$Event_Team)) == stringr::str_trim(tolower(team_name)),
      shooter    = norm_name(.data$Player_1),
      assister   = norm_name(.data$Player_2),
      shot_value = suppressWarnings(as.numeric(.data$Shot_Value)),
      shot_type  = dplyr::case_when(
        !is.na(.data$shot_value) & .data$shot_value == 1 ~ "FT",
        !is.na(.data$shot_value) & .data$shot_value == 2 ~ "2PT",
        !is.na(.data$shot_value) & .data$shot_value == 3 ~ "3PT",
        TRUE ~ NA_character_
      ),
      made = dplyr::case_when(
        tolower(.data$Event_Result) == "made"   ~ TRUE,
        tolower(.data$Event_Result) == "missed" ~ FALSE,
        TRUE ~ NA
      ),
      et = tolower(as.character(.data$Event_Type)),
      is_shot   = !is.na(.data$shot_type) & !is.na(.data$made),
      is_assist = .data$is_shot & dplyr::coalesce(.data$made, FALSE) & nzchar0(.data$assister),
      is_turnover = !is.na(.data$et) & .data$et %in% c("turnover"),
      is_steal    = !is.na(.data$et) & (.data$et %in% c("steal")),
      is_block    = !is.na(.data$et) & (.data$et %in% c("blocked shot", "block", "blockedshot")),
      is_rebound  = !is.na(.data$et) & stringr::str_detect(.data$et, "rebound")
    ) %>%
    dplyr::filter(.data$is_team) %>%
    dplyr::filter(!is.na(.data$Game_Seconds), .data$Game_Seconds >= 0) %>%
    # keep only what you need downstream, plus Home/Away + scores for hovertext
    dplyr::select(
      dplyr::any_of(c(
        "Game_Seconds","Half_Status","Time","Event_Description",
        "Event_Team","Event_Type","Event_Result","Shot_Value",
        "Player_1","Player_2",
        "Home","Away","Home_Score","Away_Score"
      ))
    )
  
  name_to_label <- shift_data %>% dplyr::distinct(.data$CleanName, .data$label)
  name_to_row   <- name_to_label %>% dplyr::left_join(row_map, by = "label")
  
  # ------------------------------------------------------------
  # SHOTS (with FT stacking)
  # ------------------------------------------------------------
  ft_stack_delta <- 0.12
  
  shots <- pbp_events %>%
    dplyr::mutate(
      shooter    = norm_name(.data$Player_1),
      assister   = norm_name(.data$Player_2),
      shot_value = suppressWarnings(as.numeric(.data$Shot_Value)),
      shot_type  = dplyr::case_when(
        !is.na(.data$shot_value) & .data$shot_value == 1 ~ "FT",
        !is.na(.data$shot_value) & .data$shot_value == 2 ~ "2PT",
        !is.na(.data$shot_value) & .data$shot_value == 3 ~ "3PT",
        TRUE ~ NA_character_
      ),
      made = dplyr::case_when(
        tolower(.data$Event_Result) == "made"   ~ TRUE,
        tolower(.data$Event_Result) == "missed" ~ FALSE,
        TRUE ~ NA
      ),
      is_shot = !is.na(.data$shot_type) & !is.na(.data$made)
    ) %>%
    dplyr::filter(.data$is_shot) %>%
    dplyr::transmute(
      Game_Seconds = suppressWarnings(as.numeric(.data$Game_Seconds)),
      CleanName    = .data$shooter,
      Half_Status  = .data$Half_Status,
      Time         = .data$Time,
      Event_Description = .data$Event_Description,
      Shot_Value   = suppressWarnings(as.numeric(.data$shot_value)),
      Event        = .data$shot_type,
      made         = .data$made,
      Home         = .data$Home,
      Away         = .data$Away,
      Home_Score   = suppressWarnings(as.numeric(.data$Home_Score)),
      Away_Score   = suppressWarnings(as.numeric(.data$Away_Score))
    ) %>%
    dplyr::left_join(name_to_row, by = "CleanName") %>%
    dplyr::filter(!is.na(.data$row), is.finite(.data$Game_Seconds)) %>%
    dplyr::mutate(
      base_y = .data$row + 0.25,
      marker_size = dplyr::if_else(.data$Shot_Value == 3, 12,
                                   dplyr::if_else(.data$Shot_Value == 2, 10, 8)),
      desc_l = stringr::str_to_lower(dplyr::coalesce(.data$Event_Description, "")),
      is_ft  = dplyr::coalesce(.data$Shot_Value, NA_real_) == 1 | .data$Event == "FT",
      ft_n   = suppressWarnings(as.integer(stringr::str_match(.data$desc_l, "\\b(\\d)\\s*of\\s*(\\d)\\b")[,2])),
      ft_m   = suppressWarnings(as.integer(stringr::str_match(.data$desc_l, "\\b(\\d)\\s*of\\s*(\\d)\\b")[,3])),
      score_line = paste0(
        dplyr::coalesce(.data$Away, "Away"), " ",
        dplyr::coalesce(as.character(.data$Away_Score), "?"),
        " - ",
        dplyr::coalesce(as.character(.data$Home_Score), "?"),
        " ",
        dplyr::coalesce(.data$Home, "Home")
      )
    ) %>%
    dplyr::group_by(.data$CleanName, .data$Game_Seconds) %>%
    dplyr::mutate(
      ft_stack_idx = dplyr::case_when(
        .data$is_ft & !is.na(.data$ft_n) ~ .data$ft_n,
        .data$is_ft ~ dplyr::row_number(),
        TRUE ~ NA_integer_
      ),
      y = dplyr::if_else(
        .data$is_ft & !is.na(.data$ft_stack_idx),
        .data$base_y + (.data$ft_stack_idx - 1L) * ft_stack_delta,
        .data$base_y
      ),
      text = paste0(
        "<b>", .data$CleanName, "</b><br>",
        .data$Event,
        dplyr::if_else(.data$is_ft & !is.na(.data$ft_n) & !is.na(.data$ft_m),
                       paste0(" ", .data$ft_n, " of ", .data$ft_m),
                       ""),
        dplyr::if_else(.data$made, " (Make)", " (Miss)", missing = ""),
        "<br>", .data$Half_Status, " · ", .data$Time,
        "<br><b>Score:</b> ", .data$score_line,
        "<br>", .data$Event_Description
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$base_y, -.data$desc_l, -.data$is_ft, -.data$ft_n, -.data$ft_m, -.data$ft_stack_idx)
  
  # ------------------------------------------------------------
  # ASSISTS / MISC (unchanged)
  # ------------------------------------------------------------
  ast <- pbp_events %>%
    dplyr::mutate(
      shooter    = norm_name(.data$Player_1),
      assister   = norm_name(.data$Player_2),
      shot_value = suppressWarnings(as.numeric(.data$Shot_Value)),
      shot_type  = dplyr::case_when(
        !is.na(.data$shot_value) & .data$shot_value == 1 ~ "FT",
        !is.na(.data$shot_value) & .data$shot_value == 2 ~ "2PT",
        !is.na(.data$shot_value) & .data$shot_value == 3 ~ "3PT",
        TRUE ~ NA_character_
      ),
      made = dplyr::case_when(
        tolower(.data$Event_Result) == "made"   ~ TRUE,
        tolower(.data$Event_Result) == "missed" ~ FALSE,
        TRUE ~ NA
      ),
      is_shot   = !is.na(.data$shot_type) & !is.na(.data$made),
      is_assist = .data$is_shot & dplyr::coalesce(.data$made, FALSE) & nzchar0(.data$assister)
    ) %>%
    dplyr::filter(.data$is_assist) %>%
    dplyr::transmute(
      Game_Seconds = .data$Game_Seconds,
      CleanName    = .data$assister,
      Half_Status  = .data$Half_Status,
      Time         = .data$Time,
      Event_Description = .data$Event_Description,
      Shot_Value   = .data$shot_value,
      Event        = "Assist",
      made         = NA,
      Home         = .data$Home,
      Away         = .data$Away,
      Home_Score   = suppressWarnings(as.numeric(.data$Home_Score)),
      Away_Score   = suppressWarnings(as.numeric(.data$Away_Score))
    ) %>%
    dplyr::left_join(name_to_row, by = "CleanName") %>%
    dplyr::filter(!is.na(.data$row)) %>%
    dplyr::mutate(
      y = .data$row + 0.25,
      score_line = paste0(
        dplyr::coalesce(.data$Away, "Away"), " ",
        dplyr::coalesce(as.character(.data$Away_Score), "?"),
        " - ",
        dplyr::coalesce(as.character(.data$Home_Score), "?"),
        " ",
        dplyr::coalesce(.data$Home, "Home")
      ),
      text = paste0(
        "<b>", .data$CleanName, "</b><br>Assist<br>",
        .data$Half_Status, " · ", .data$Time,
        "<br><b>Score:</b> ", .data$score_line,
        "<br>", .data$Event_Description
      ),
      marker_size = dplyr::if_else(.data$Shot_Value == 3, 12,
                                   dplyr::if_else(.data$Shot_Value == 2, 10, 8))
    )
  
  misc <- pbp_events %>%
    dplyr::mutate(
      shooter = norm_name(.data$Player_1),
      et      = tolower(as.character(.data$Event_Type)),
      is_turnover = !is.na(.data$et) & .data$et %in% c("turnover"),
      is_steal    = !is.na(.data$et) & (.data$et %in% c("steal")),
      is_block    = !is.na(.data$et) & (.data$et %in% c("blocked shot", "block", "blockedshot")),
      is_rebound  = !is.na(.data$et) & stringr::str_detect(.data$et, "rebound")
    ) %>%
    dplyr::filter(.data$is_turnover | .data$is_steal | .data$is_block | .data$is_rebound) %>%
    dplyr::transmute(
      Game_Seconds = .data$Game_Seconds,
      CleanName    = .data$shooter,
      Half_Status  = .data$Half_Status,
      Time         = .data$Time,
      Event_Description = .data$Event_Description,
      Event = dplyr::case_when(
        .data$is_turnover ~ "Turnover",
        .data$is_steal    ~ "Steal",
        .data$is_block    ~ "Block",
        .data$is_rebound  ~ "Rebound",
        TRUE              ~ NA_character_
      ),
      Home       = .data$Home,
      Away       = .data$Away,
      Home_Score = suppressWarnings(as.numeric(.data$Home_Score)),
      Away_Score = suppressWarnings(as.numeric(.data$Away_Score))
    ) %>%
    dplyr::left_join(name_to_row, by = "CleanName") %>%
    dplyr::filter(!is.na(.data$row), !is.na(.data$Event)) %>%
    dplyr::mutate(
      y = .data$row - 0.25,
      score_line = paste0(
        dplyr::coalesce(.data$Away, "Away"), " ",
        dplyr::coalesce(as.character(.data$Away_Score), "?"),
        " - ",
        dplyr::coalesce(as.character(.data$Home_Score), "?"),
        " ",
        dplyr::coalesce(.data$Home, "Home")
      ),
      text = paste0(
        "<b>", .data$CleanName, "</b><br>", .data$Event, "<br>",
        .data$Half_Status, " · ", .data$Time,
        "<br><b>Score:</b> ", .data$score_line,
        "<br>", .data$Event_Description
      )
    )
  
  seg_text <- seg %>%
    dplyr::transmute(
      x = (.data$start + .data$end) / 2,
      y = .data$row,
      text = ifelse(is.na(.data$team_pm), "", as.character(.data$team_pm))
    )
  
  y_ticks <- row_map$row
  y_text  <- shift_data %>%
    dplyr::distinct(.data$label, .data$CleanName) %>%
    dplyr::right_join(row_map, by = "label") %>%
    dplyr::arrange(.data$row) %>%
    dplyr::pull(.data$CleanName)

  pal <- get_event_palette(dark_mode)
  shot_make_color <- pal$shot_make
  shot_miss_color <- pal$shot_miss
  block_color <- pal$block
  # ADDED: Extract assist color from palette for dark mode visibility
  assist_color <- pal$assist
  
  
  
  p <- plotly::plot_ly()
  
  if (nrow(seg) > 0) {
    for (i in seq_len(nrow(seg))) {
      p <- p %>%
        plotly::add_trace(
          type = "scatter", mode = "lines",
          x = c(seg$start[i], seg$end[i]),
          y = c(seg$y[i],     seg$y[i]),
          line = list(color = seg$col[i], width = 28),
          hoverinfo = "skip",
          showlegend = FALSE
        ) %>%
        plotly::add_trace(
          type = "scatter", mode = "lines",
          x = c(seg$start[i], seg$end[i]),
          y = c(seg$y[i],     seg$y[i]),
          line = list(color = "gray30", width = 2),
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
  }
  
  # MODIFIED: Increased textfont size from 14 to 18 for better readability
  p <- p %>%
    plotly::add_text(
      data = seg_text,
      x = ~x, y = ~y, text = ~text,
      textfont = list(size = 18, color = "black", family = "Arial Black"),
      hoverinfo = "skip",
      showlegend = FALSE
    ) %>%
    plotly::add_trace(
      type = "scatter", mode = "lines",
      x = c(halftime, halftime),
      y = c(0.5, max_row + 0.5),
      line = list(color = "gray50", dash = "dash", width = 2),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  
  s_make <- shots %>% dplyr::filter(.data$made)
  if (nrow(s_make)) {
    p <- p %>% plotly::add_markers(
      data = s_make,
      x = ~Game_Seconds, y = ~y,
      marker = list(symbol = "circle", color = shot_make_color, size = s_make$marker_size,
                    line = list(color = shot_make_color, width = 1)),
      text = ~text, hoverinfo = "text",
      name = "Shot (Make)", showlegend = TRUE
    )
  }
  
  s_miss <- shots %>% dplyr::filter(!.data$made)
  if (nrow(s_miss)) {
    p <- p %>% plotly::add_markers(
      data = s_miss,
      x = ~Game_Seconds, y = ~y,
      marker = list(symbol = "circle-open", color = shot_miss_color, size = s_miss$marker_size,
                    line = list(color = shot_miss_color, width = 2)),
      text = ~text, hoverinfo = "text",
      name = "Shot (Miss)", showlegend = TRUE
    )
  }
  
  # MODIFIED: Assist markers now use palette color for dark mode visibility
  if (nrow(ast)) {
    p <- p %>% plotly::add_markers(
      data = ast,
      x = ~Game_Seconds, y = ~y,
      marker = list(symbol = "triangle-up-open", color = assist_color, size = ast$marker_size,
                    line = list(color = assist_color, width = 2)),
      text = ~text, hoverinfo = "text",
      name = "Assist", showlegend = TRUE
    )
  }
  
  add_misc <- function(p, df, event_name, symbol, color, line_color = color, line_width = 2) {
    dd <- df %>% dplyr::filter(.data$Event == event_name)
    if (nrow(dd) == 0) return(p)
    
    p %>% plotly::add_markers(
      data = dd,
      x = ~Game_Seconds, y = ~y,
      marker = list(
        symbol = symbol,
        color  = color,                  # fill (matters for filled symbols)
        size   = 10,
        line   = list(color = line_color, width = line_width)  # stroke (matters for asterisk/x/etc)
      ),
      text = ~text, hoverinfo = "text",
      name = event_name, showlegend = TRUE
    )
  }
  
  p <- add_misc(p, misc, "Turnover", "x",        color = "red",     line_color = "red",     line_width = 2)
  p <- add_misc(p, misc, "Steal",    "asterisk", color = "#FFD700", line_color = "#FFD700", line_width = 3)
  p <- add_misc(p, misc, "Block",    "square-open", color = block_color, line_color = block_color, line_width = 2)
  p <- add_misc(p, misc, "Rebound",  "diamond",  color = "steelblue", line_color = "black", line_width = 1)
  
  
  p %>%
    plotly::layout(
      title = list(
        text = paste0(team_name, " Player Shift Chart<br><sup>Final Score: ", final_score, "</sup>"),
        x = 0.02,
        xanchor = "left"
      ),
      xaxis = list(
        range = c(-220, final_second),
        tickvals = c(0, 600, 1200, 1800, 2400),
        ticktext = c("0:00", "10:00", "Halftime", "30:00", "End Regulation"),
        title = ""
      ),
      yaxis = list(
        tickmode = "array",
        tickvals = y_ticks,
        ticktext = y_text,
        autorange = "reversed",
        title = ""
      ),
      legend = list(
        orientation = "h",
        x = 0.98, xanchor = "right",
        y = 1.12, yanchor = "top"
      ),
      margin = list(l = 100, r = 80, t = 110, b = 60)
    )
}

normalize_sched_for_timeline <- function(sched_df) {
  stopifnot(is.data.frame(sched_df))
  
  if (!"game_id" %in% names(sched_df)) {
    if ("Game_ID" %in% names(sched_df)) sched_df$game_id <- as.character(sched_df$Game_ID)
  }
  if (!"game_date" %in% names(sched_df)) {
    if ("Date" %in% names(sched_df)) sched_df$game_date <- coerce_to_date(sched_df$Date)
  }
  if (!"game_lbl" %in% names(sched_df)) {
    if ("label" %in% names(sched_df)) sched_df$game_lbl <- as.character(sched_df$label) else sched_df$game_lbl <- NA_character_
  }
  if (!"final_score" %in% names(sched_df)) {
    if ("final_score" %in% names(sched_df)) {
      # no-op
    } else if ("Final_Score" %in% names(sched_df)) {
      sched_df$final_score <- as.character(sched_df$Final_Score)
    } else {
      sched_df$final_score <- NA_character_
    }
  }
  
  sched_df %>%
    dplyr::mutate(
      game_id   = as.character(.data$game_id),
      game_date = coerce_to_date(.data$game_date),
      game_lbl  = as.character(.data$game_lbl),
      final_score = as.character(.data$final_score)
    ) %>%
    dplyr::filter(!is.na(.data$game_id) & nzchar(.data$game_id)) %>%
    dplyr::arrange(.data$game_date, .data$game_id) %>%
    dplyr::select(game_id, game_date, game_lbl, final_score)
}

# ==============================================================================
# SEASON TIMELINE: data builder (returns list(shifts=..., events=...))
# - pulls per-game box stats
# - pulls per-game PBP markers (selected player only)
# ==============================================================================
# ------------------------------------------------------------------------------
# FIX: your parse error is from mismatched braces in get_one_game(), plus you
# pasted a "PATCH" block at the bottom of the file that includes a stray "}"
# outside any function.
#
# 1) Replace your CURRENT build_season_shift_timeline_data() with the version
#    below (brace-balanced).
# 2) DELETE the entire bottom "PATCH inside build_season_shift_timeline_data()"
#    snippet from your file (it is top-level code and will break parsing).
# ------------------------------------------------------------------------------

# ==============================================================================
# SEASON TIMELINE: data builder (DI-safe)
# Keep ONLY ONE copy of this function in plotly_helpers.R
# ==============================================================================


# ==============================================================================
# Season/window base precompute (NO player) + cheap player filtering
# ==============================================================================

build_season_window_base <- function(
    season,
    team_name,
    sched_df,
    teamids,
    pbp_dir = NULL,
    pbp_loader_many = NULL,        # function(game_ids, season, pbp_dir, verbose) -> data.frame
    pbp_standardizer = NULL,       # optional: function(pbp_all) -> pbp_all
    shift_builder_one_game = NULL, # function(game_id, team_name, teamids, season, pbp) -> shift_df (ALL players)
    game_ids = NULL,               # optional restriction vector
    verbose = FALSE
) {
  nzchar_vec <- function(x) !is.na(x) & nzchar(as.character(x))
  
  ensure_cols <- function(df, cols, fill = NA) {
    for (cc in cols) if (!cc %in% names(df)) df[[cc]] <- fill
    df
  }
  
  stopifnot(is.data.frame(sched_df))
  if (!("Game_ID" %in% names(sched_df)) && !("game_id" %in% names(sched_df))) stop("sched_df must contain Game_ID or game_id")
  if (!("Date" %in% names(sched_df)) && !("game_date" %in% names(sched_df))) stop("sched_df must contain Date or game_date")
  
  if (is.null(pbp_dir) || !nzchar(as.character(pbp_dir))) pbp_dir <- file.path("cache", "pbp")
  if (!is.function(pbp_loader_many))        stop("pbp_loader_many must be a function")
  if (!is.function(shift_builder_one_game)) stop("shift_builder_one_game must be a function")
  
  # ---- normalize schedule ----------------------------------------------------
  sched2 <- sched_df
  
  if (!"game_id" %in% names(sched2)) {
    sched2$game_id <- as.character(sched2$Game_ID)
  } else {
    sched2$game_id <- as.character(sched2$game_id)
  }
  
  if (!"game_date" %in% names(sched2)) {
    sched2$game_date <- coerce_to_date(sched2$Date)
  } else {
    sched2$game_date <- coerce_to_date(sched2$game_date)
  }
  
  sched2$game_lbl <- if ("label" %in% names(sched2)) as.character(sched2$label) else NA_character_
  
  if (!"final_score" %in% names(sched2)) {
    sched2$final_score <- dplyr::case_when(
      "Final_Score" %in% names(sched2) ~ as.character(sched2$Final_Score),
      "final"       %in% names(sched2) ~ as.character(sched2$final),
      "Score"       %in% names(sched2) ~ as.character(sched2$Score),
      "score"       %in% names(sched2) ~ as.character(sched2$score),
      TRUE ~ NA_character_
    )
  }
  
  sched2 <- sched2 %>%
    dplyr::filter(nzchar_vec(.data$game_id)) %>%
    dplyr::arrange(.data$game_date, .data$game_id)
  
  if (nrow(sched2) == 0) {
    return(list(
      sched = tibble::tibble(),
      pbp   = dplyr::tibble(),
      shifts_all = tibble::tibble(),
      events_index = tibble::tibble()
    ))
  }
  
  if (!is.null(game_ids) && length(game_ids) > 0) {
    gids <- as.character(game_ids)
    gids <- gids[nzchar_vec(gids)]
    sched2 <- sched2 %>% dplyr::filter(.data$game_id %in% gids)
    if (nrow(sched2) == 0) {
      if (isTRUE(verbose)) message("[window] schedule filtered to 0 games after game_ids restriction")
      return(list(
        sched = tibble::tibble(),
        pbp   = dplyr::tibble(),
        shifts_all = tibble::tibble(),
        events_index = tibble::tibble()
      ))
    }
  }
  
  # ---- bulk load pbp ---------------------------------------------------------
  pbp_all <- tryCatch(
    pbp_loader_many(
      game_ids = sched2$game_id,
      season   = season,
      pbp_dir  = pbp_dir,
      verbose  = FALSE
    ),
    error = function(e) {
      message("[window] bulk load ERROR: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.function(pbp_standardizer)) {
    pbp_all <- tryCatch(pbp_standardizer(pbp_all), error = function(e) pbp_all)
  }
  
  if (is.null(pbp_all) || !is.data.frame(pbp_all) || nrow(pbp_all) == 0) {
    if (isTRUE(verbose)) message("[window] bulk pbp: MISSING/EMPTY")
    return(list(
      sched = sched2,
      pbp   = dplyr::tibble(),
      shifts_all = tibble::tibble(),
      events_index = tibble::tibble()
    ))
  }
  
  message("pbp_all")
  # glimpse(pbp_all)
  
  gid_col <- intersect(c("ID", "Game_ID", "game_id", "GameId", "gameId"), names(pbp_all))[1]
  if (is.na(gid_col) || !nzchar(gid_col)) stop("PBP missing a game id column after bulk load")
  
  # ---- shifts (ALL players) per game (cached by Shiny bindCache upstream) ----
  get_shifts_one_game <- function(gid) {
    pbp_g <- pbp_all %>% dplyr::filter(.data[[gid_col]] == as.character(gid))
    if (!is.data.frame(pbp_g) || nrow(pbp_g) == 0) return(NULL)
    
    sd <- tryCatch(
      shift_builder_one_game(
        game_id   = gid,
        team_name = team_name,
        teamids   = teamids,
        season    = season,
        pbp       = pbp_g
      ),
      error = function(e) {
        message("[window] gid=", gid, " | shift builder ERROR: ", conditionMessage(e))
        NULL
      }
    )
    
    if (!is.data.frame(sd) || nrow(sd) == 0) return(NULL)
    
    # enforce minimal schema used later
    if (!"game_id" %in% names(sd)) sd$game_id <- as.character(gid)
    if (!"CleanName" %in% names(sd) && "player" %in% names(sd)) sd$CleanName <- norm_name(sd$player)
    
    sd
  }
  
  shifts_list <- purrr::map(sched2$game_id, get_shifts_one_game)
  shifts_list <- shifts_list[!vapply(shifts_list, is.null, logical(1))]
  shifts_all  <- purrr::map_dfr(shifts_list, ~ .x)
  
  if (!is.data.frame(shifts_all) || nrow(shifts_all) == 0) {
    return(list(
      sched = sched2,
      pbp   = pbp_all,
      shifts_all = tibble::tibble(),
      events_index = tibble::tibble()
    ))
  }
  
  # ---- attach schedule labels/dates to shifts (once) --------------------------
  if (!"Date_chr" %in% names(sched2)) {
    sched2$Date_chr <- if ("Date" %in% names(sched2)) as.character(sched2$Date) else NA_character_
  }
  
  sched_join <- sched2 %>%
    dplyr::select(game_id, game_date, Date = Date_chr, game_lbl, final_score)
  
  shifts_all <- shifts_all %>%
    dplyr::left_join(sched_join, by = "game_id") %>%
    dplyr::mutate(
      game_date = dplyr::coalesce(.data$game_date, coerce_to_date(.data$Date)),
      game_label = dplyr::if_else(
        !is.na(.data$game_lbl) & nzchar_vec(.data$game_lbl),
        .data$game_lbl,
        dplyr::if_else(!is.na(.data$game_date), format(.data$game_date, "%Y-%m-%d"), .data$game_id)
      )
    ) %>%
    dplyr::arrange(.data$game_date, .data$game_id)
  
  # ---- events index (player-agnostic): keep rows where Player_1/Player_2 present
  # This keeps the expensive load/standardize in base, then player filtering is cheap.
  events_index <- pbp_all %>%
    dplyr::transmute(
      game_id = as.character(.data[[gid_col]]),
      Game_Seconds = suppressWarnings(as.numeric(.data$Game_Seconds)),
      Time = as.character(.data$Time),
      Half_Status = as.character(.data$Half_Status),
      Event_Type = as.character(.data$Event_Type),
      Event_Result = as.character(.data$Event_Result),
      Event_Description = as.character(.data$Event_Description),
      Event_Team = as.character(.data$Event_Team),
      Player_1 = as.character(.data$Player_1),
      Player_2 = as.character(.data$Player_2),
      Home = as.character(.data$Home),
      Away = as.character(.data$Away),
      Home_Score = suppressWarnings(as.numeric(.data$Home_Score)),
      Away_Score = suppressWarnings(as.numeric(.data$Away_Score)),
      Home.1 = as.character(.data$Home.1),
      Home.2 = as.character(.data$Home.2),
      Home.3 = as.character(.data$Home.3),
      Home.4 = as.character(.data$Home.4),
      Home.5 = as.character(.data$Home.5),
      Away.1 = as.character(.data$Away.1),
      Away.2 = as.character(.data$Away.2),
      Away.3 = as.character(.data$Away.3),
      Away.4 = as.character(.data$Away.4),
      Away.5 = as.character(.data$Away.5),
      Shot_Value = suppressWarnings(as.numeric(.data$Shot_Value))
    ) %>%
    dplyr::filter(
      is.finite(.data$Game_Seconds),
      (!is.na(.data$Player_1) & nzchar(trimws(.data$Player_1))) |
        (!is.na(.data$Player_2) & nzchar(trimws(.data$Player_2))) |
        (!is.na(.data$Event_Description) & nzchar(trimws(.data$Event_Description)))
    ) %>%
    dplyr::left_join(
      shifts_all %>% dplyr::distinct(.data$game_id, .data$game_date, .data$game_label),
      by = "game_id"
    )
  
  events_index <- add_assist_markers_from_shots(events_index)
  
  message("events_index")
  # glimpse(events_index)
  
  
  diag_assist_rows <- function(events_index) {
    if (is.null(events_index) || !is.data.frame(events_index) || nrow(events_index) == 0) return(invisible(NULL))
    
    et <- if ("Event_Type" %in% names(events_index)) tolower(trimws(dplyr::coalesce(as.character(events_index[["Event_Type"]]), ""))) else character(0)
    n_ast_type <- sum(et == "assist", na.rm = TRUE)
    
    n_syn <- if ("synthetic_ast" %in% names(events_index)) sum(isTRUE(events_index[["synthetic_ast"]]), na.rm = TRUE) else NA_integer_
    
    has_lbl <- "game_label" %in% names(events_index)
    n_ast_lbl <- if (has_lbl) sum(et == "assist" & !is.na(events_index[["game_label"]]) & nzchar(events_index[["game_label"]]), na.rm = TRUE) else NA_integer_
    
    message("[base][diag] events_index rows=", nrow(events_index),
            " | Event_Type=='assist': ", n_ast_type,
            " | synthetic_ast TRUE: ", n_syn,
            " | assist rows with game_label: ", n_ast_lbl)
    
    invisible(NULL)
  }
  
  stat_cols <- c("PTS","REB","AST","TOV","STL","BLK")
  shifts_all <- ensure_cols(shifts_all, stat_cols, fill = NA_integer_)
  
  list(
    sched = sched2,
    pbp   = pbp_all,
    shifts_all = shifts_all,
    events_index = events_index
  )
}

# ------------------------------------------------------------------------------
# Goal: show BOTH
#   - SHOT marker for shooter (Player_1)
#   - AST marker for assister (Player_2)
#
# Approach:
#   1) Keep original shot rows unchanged (so shooter markers work).
#   2) Create "shadow" assist rows from assisted shot rows (Player_2 present),
#      with Event/Event_Type = "assist" and Shot_Value cleared so they never
#      get classified as shots.
#   3) Bind shadow rows onto events_index ONCE (in your base builder), so your
#      existing per-player filter + tag_timeline_events() will pick them up.
#
# This obviates trying to infer AST markers from shot rows at plot time.
# ------------------------------------------------------------------------------

add_assist_markers_from_shots <- function(events_index) {
  if (is.null(events_index) || !is.data.frame(events_index) || nrow(events_index) == 0) {
    return(events_index)
  }
  
  n0 <- nrow(events_index)
  
  # Safe pulls
  p1 <- if ("Player_1" %in% names(events_index)) as.character(events_index[["Player_1"]]) else rep(NA_character_, n0)
  p2 <- if ("Player_2" %in% names(events_index)) as.character(events_index[["Player_2"]]) else rep(NA_character_, n0)
  
  et   <- if ("Event_Type" %in% names(events_index)) tolower(trimws(dplyr::coalesce(as.character(events_index[["Event_Type"]]), ""))) else rep("", n0)
  er   <- if ("Event_Result" %in% names(events_index)) tolower(trimws(dplyr::coalesce(as.character(events_index[["Event_Result"]]), ""))) else rep("", n0)
  desc <- if ("Event_Description" %in% names(events_index)) tolower(dplyr::coalesce(as.character(events_index[["Event_Description"]]), "")) else rep("", n0)
  
  sv <- if ("Shot_Value" %in% names(events_index)) suppressWarnings(as.numeric(events_index[["Shot_Value"]])) else rep(NA_real_, n0)
  
  has_p2 <- !is.na(p2) & nzchar(trimws(p2))
  
  # Identify shot-ish rows (same idea as your filter; intentionally broad)
  is_shot_row <- (!is.na(sv) & sv %in% c(1, 2, 3)) |
    grepl("\\b(2pt|3pt|ft)\\b", et) |
    grepl("\\b(2pt|3pt|ft)\\b", er) |
    grepl("free throw|three|3\\s*pt|two\\-?point|jumper|jump shot|layup|dunk|made|miss", desc)
  
  idx <- which(is_shot_row & has_p2)
  if (length(idx) == 0) return(events_index)
  
  ast_rows <- events_index[idx, , drop = FALSE]
  
  # Ensure required columns exist before assignment
  need_cols <- c(
    "Event", "Event_Type", "Event_Result", "Event_Description",
    "Shot_Value", "made", "Player"
  )
  for (cc in need_cols) {
    if (!cc %in% names(ast_rows)) ast_rows[[cc]] <- NA
  }
  
  # Make these rows unambiguously "assist" and NOT shots
  ast_rows[["Event"]]            <- rep("assist", nrow(ast_rows))
  ast_rows[["Event_Type"]]       <- rep("assist", nrow(ast_rows))
  ast_rows[["Event_Result"]]     <- rep(NA_character_, nrow(ast_rows))
  ast_rows[["Shot_Value"]]       <- rep(NA_real_, nrow(ast_rows))
  ast_rows[["made"]]             <- rep(NA_integer_, nrow(ast_rows))
  
  # Keep Player_2 as the assister (so your filter matches on Player_2 for AST rows)
  # Also set Player to the assister so hover shows the right name even before tagging.
  ast_rows[["Player"]] <- as.character(ast_rows[["Player_2"]])
  
  # Make description non-shotty so it never gets mis-tagged as SHOT by text
  # (Avoid words like "made/miss/three" etc.)
  shooter_txt <- if ("Player_1" %in% names(ast_rows)) dplyr::coalesce(as.character(ast_rows[["Player_1"]]), "") else ""
  ast_rows[["Event_Description"]] <- ifelse(
    nzchar(shooter_txt),
    paste0("Assist (to ", shooter_txt, ")"),
    "Assist"
  )
  
  # De-dup protection: if the upstream feed already has separate assist rows,
  # you may create duplicates. Mark these as synthetic and allow callers to drop if desired.
  if (!"synthetic_ast" %in% names(events_index)) {
    events_index[["synthetic_ast"]] <- FALSE
  }
  if (!"synthetic_ast" %in% names(ast_rows)) {
    ast_rows[["synthetic_ast"]] <- TRUE
  } else {
    ast_rows[["synthetic_ast"]] <- TRUE
  }
  
  dplyr::bind_rows(events_index, ast_rows)
}

# ------------------------------------------------------------------------------
# IMPORTANT: Call add_assist_markers_from_shots() ONCE, when you build base$events_index
# (i.e., in build_season_window_base after you assemble events_index)
#
# Example patch inside build_season_window_base():
#
#   events_index <- ... # your existing combined events table across games
#   events_index <- standardize_pbp_schema(events_index) # if you do schema work here
#   events_index <- add_assist_markers_from_shots(events_index)
#   base$events_index <- events_index
#
# Then your existing build_season_shift_timeline_bundle_fast():
#   ev_player <- filter_events_for_player_from_index(base$events_index, player_raw)
#   ev_player <- tag_timeline_events(ev_player)
# will now produce:
#   - shooter gets SHOT markers (from original rows)
#   - assister gets AST markers (from synthetic rows)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# OPTIONAL (but recommended): tighten assist detection in filter_events_for_player_from_index
# so shot rows containing "assist" text never get treated as pure assist rows.
# Keep your current function, but ensure:
#   is_ast_row <- ... & (!is_shot_row)
#   is_ast_o   <- ... & (!is_shot_o)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FIX: events_all is built from events_builder_one_game(pbp, ...) so it will NOT
# automatically contain Home.1..Away.5 unless you add them there (or join them in).
#
# Easiest, robust approach:
#   1) Build a per-row lookup table from pbp_all that includes lineup + score cols.
#   2) LEFT JOIN that onto events_all by (game_id, Game_Seconds, Time, Half_Status,
#      Event_Description, Player_1, Player_2, Event_Type, Event_Result, Event_Team, Shot_Value)
#      with a row_id fallback to avoid many-to-many blowups.
#
# Drop-in helper + where to call it (near where you currently join game_label/date).
# ------------------------------------------------------------------------------

augment_events_all_with_pbp_context <- function(events_all, pbp_all, gid_col) {
  if (is.null(events_all) || !is.data.frame(events_all) || nrow(events_all) == 0) return(events_all)
  if (is.null(pbp_all)   || !is.data.frame(pbp_all)   || nrow(pbp_all)   == 0) return(events_all)
  if (is.null(gid_col) || is.na(gid_col) || !nzchar(as.character(gid_col))) return(events_all)
  
  nzchar0 <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
  pick_any <- function(df, nms) intersect(nms, names(df))
  
  # --- ensure game_id exists on both sides -----------------------------------
  if (!"game_id" %in% names(events_all)) {
    # nothing to join on; return as-is
    return(events_all)
  }
  
  # --- pbp context: only the columns we need + row_id for stable fallback -----
  ctx_cols <- c(
    gid_col,
    "Game_Seconds","Time","Half_Status","Event_Type","Event_Result","Event_Description","Event_Team",
    "Player_1","Player_2","Shot_Value",
    "Home","Away","Home_Score","Away_Score",
    "Home.1","Home.2","Home.3","Home.4","Home.5",
    "Away.1","Away.2","Away.3","Away.4","Away.5"
  )
  ctx_cols <- pick_any(pbp_all, ctx_cols)
  
  pbp_ctx <- pbp_all %>%
    dplyr::transmute(
      game_id = as.character(.data[[gid_col]]),
      row_id  = dplyr::row_number(),
      Game_Seconds = suppressWarnings(as.numeric(.data$Game_Seconds)),
      Time         = if ("Time" %in% names(pbp_all)) as.character(.data$Time) else NA_character_,
      Half_Status  = if ("Half_Status" %in% names(pbp_all)) as.character(.data$Half_Status) else NA_character_,
      Event_Type   = if ("Event_Type" %in% names(pbp_all)) as.character(.data$Event_Type) else NA_character_,
      Event_Result = if ("Event_Result" %in% names(pbp_all)) as.character(.data$Event_Result) else NA_character_,
      Event_Description = if ("Event_Description" %in% names(pbp_all)) as.character(.data$Event_Description) else NA_character_,
      Event_Team   = if ("Event_Team" %in% names(pbp_all)) as.character(.data$Event_Team) else NA_character_,
      Player_1     = if ("Player_1" %in% names(pbp_all)) as.character(.data$Player_1) else NA_character_,
      Player_2     = if ("Player_2" %in% names(pbp_all)) as.character(.data$Player_2) else NA_character_,
      Shot_Value   = if ("Shot_Value" %in% names(pbp_all)) suppressWarnings(as.numeric(.data$Shot_Value)) else NA_real_,
      Home         = if ("Home" %in% names(pbp_all)) as.character(.data$Home) else NA_character_,
      Away         = if ("Away" %in% names(pbp_all)) as.character(.data$Away) else NA_character_,
      Home_Score   = if ("Home_Score" %in% names(pbp_all)) suppressWarnings(as.numeric(.data$Home_Score)) else NA_real_,
      Away_Score   = if ("Away_Score" %in% names(pbp_all)) suppressWarnings(as.numeric(.data$Away_Score)) else NA_real_,
      Home.1       = if ("Home.1" %in% names(pbp_all)) as.character(.data$Home.1) else NA_character_,
      Home.2       = if ("Home.2" %in% names(pbp_all)) as.character(.data$Home.2) else NA_character_,
      Home.3       = if ("Home.3" %in% names(pbp_all)) as.character(.data$Home.3) else NA_character_,
      Home.4       = if ("Home.4" %in% names(pbp_all)) as.character(.data$Home.4) else NA_character_,
      Home.5       = if ("Home.5" %in% names(pbp_all)) as.character(.data$Home.5) else NA_character_,
      Away.1       = if ("Away.1" %in% names(pbp_all)) as.character(.data$Away.1) else NA_character_,
      Away.2       = if ("Away.2" %in% names(pbp_all)) as.character(.data$Away.2) else NA_character_,
      Away.3       = if ("Away.3" %in% names(pbp_all)) as.character(.data$Away.3) else NA_character_,
      Away.4       = if ("Away.4" %in% names(pbp_all)) as.character(.data$Away.4) else NA_character_,
      Away.5       = if ("Away.5" %in% names(pbp_all)) as.character(.data$Away.5) else NA_character_
    ) %>%
    dplyr::filter(is.finite(.data$Game_Seconds))
  
  # --- add join keys + row_id placeholder on events_all -----------------------
  if (!"row_id" %in% names(events_all)) events_all$row_id <- NA_integer_
  
  # normalize types so joins behave
  events_all2 <- events_all %>%
    dplyr::mutate(
      game_id      = as.character(.data$game_id),
      Game_Seconds = if ("Game_Seconds" %in% names(events_all)) suppressWarnings(as.numeric(.data$Game_Seconds)) else NA_real_,
      Time         = if ("Time" %in% names(events_all)) as.character(.data$Time) else NA_character_,
      Half_Status  = if ("Half_Status" %in% names(events_all)) as.character(.data$Half_Status) else NA_character_,
      Event_Type   = if ("Event_Type" %in% names(events_all)) as.character(.data$Event_Type) else NA_character_,
      Event_Result = if ("Event_Result" %in% names(events_all)) as.character(.data$Event_Result) else NA_character_,
      Event_Description = if ("Event_Description" %in% names(events_all)) as.character(.data$Event_Description) else NA_character_,
      Event_Team   = if ("Event_Team" %in% names(events_all)) as.character(.data$Event_Team) else NA_character_,
      Player_1     = if ("Player_1" %in% names(events_all)) as.character(.data$Player_1) else NA_character_,
      Player_2     = if ("Player_2" %in% names(events_all)) as.character(.data$Player_2) else NA_character_,
      Shot_Value   = if ("Shot_Value" %in% names(events_all)) suppressWarnings(as.numeric(.data$Shot_Value)) else NA_real_
    )
  
  # if events_all doesn't have the raw PBP keys, this join cannot work
  need_keys <- c("game_id","Game_Seconds","Event_Description")
  if (!all(need_keys %in% names(events_all2))) return(events_all)
  
  # --- primary join (rich keys) ----------------------------------------------
  joined <- suppressWarnings(
    dplyr::left_join(
      events_all2,
      pbp_ctx %>%
        dplyr::select(
          game_id, row_id,
          Game_Seconds, Time, Half_Status, Event_Type, Event_Result, Event_Description, Event_Team,
          Player_1, Player_2, Shot_Value,
          Home, Away, Home_Score, Away_Score,
          Home.1, Home.2, Home.3, Home.4, Home.5,
          Away.1, Away.2, Away.3, Away.4, Away.5
        ),
      by = c(
        "game_id",
        "Game_Seconds",
        "Time",
        "Half_Status",
        "Event_Type",
        "Event_Result",
        "Event_Description",
        "Event_Team",
        "Player_1",
        "Player_2",
        "Shot_Value"
      ),
      relationship = "many-to-many"
    )
  )
  
  # --- disambiguate many-to-many by choosing closest row_id within each key ----
  # If duplicates occur, keep the first matched row_id per original row order.
  joined <- joined %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "game_id","Game_Seconds","Time","Half_Status","Event_Type","Event_Result",
      "Event_Description","Event_Team","Player_1","Player_2","Shot_Value"
    )))) %>%
    dplyr::mutate(.match_rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.na(.data$.match_rank) | .data$.match_rank == 1L) %>%
    dplyr::select(-.data$.match_rank)
  
  joined
}


filter_events_for_player_from_index <- function(events_index, player_raw) {
  if (is.null(events_index) || !is.data.frame(events_index) || nrow(events_index) == 0) {
    return(tibble::tibble())
  }
  
  sel_norm <- norm_name(player_raw)
  sel_base <- base_name(player_raw)
  
  n0 <- nrow(events_index)
  
  p1 <- if ("Player_1" %in% names(events_index)) as.character(events_index[["Player_1"]]) else rep(NA_character_, n0)
  p2 <- if ("Player_2" %in% names(events_index)) as.character(events_index[["Player_2"]]) else rep(NA_character_, n0)
  
  p1n <- norm_name(p1); p2n <- norm_name(p2)
  p1b <- base_name(p1); p2b <- base_name(p2)
  
  et   <- if ("Event_Type" %in% names(events_index)) tolower(trimws(dplyr::coalesce(as.character(events_index[["Event_Type"]]), ""))) else rep("", n0)
  er   <- if ("Event_Result" %in% names(events_index)) tolower(trimws(dplyr::coalesce(as.character(events_index[["Event_Result"]]), ""))) else rep("", n0)
  desc <- if ("Event_Description" %in% names(events_index)) tolower(dplyr::coalesce(as.character(events_index[["Event_Description"]]), "")) else rep("", n0)
  
  has_p2 <- !is.na(p2) & nzchar(trimws(p2))
  
  is_shot_row <- grepl("\\b(2pt|3pt|ft)\\b", et) |
    grepl("\\b(2pt|3pt|ft)\\b", er) |
    grepl("free throw|three|3\\s*pt|two\\-?point|jumper|jump shot|layup|dunk|made|miss", desc)
  
  # CRITICAL: if a row is shot-like, do NOT treat it as an assist row for filtering.
  # (Synthetic assist rows won’t be shot-like because we blank Shot_Value and remove shot text.)
  is_ast_row <- (grepl("\\bassist\\b", et) | grepl("\\bassist\\b", er) | (grepl("\\bassist\\b", desc) & has_p2)) & (!is_shot_row)
  
  match_p1 <- (p1n == sel_norm) | (p1b == sel_base)
  match_p2 <- (p2n == sel_norm) | (p2b == sel_base)
  
  keep <- dplyr::case_when(
    is_ast_row  ~ match_p2,               # assister
    is_shot_row ~ match_p1,               # shooter
    TRUE        ~ (match_p1 | match_p2)   # fallback
  )
  
  out <- events_index[keep, , drop = FALSE]
  if (!is.data.frame(out) || nrow(out) == 0) return(tibble::tibble())
  
  n <- nrow(out)
  if (!"Player" %in% names(out)) out[["Player"]] <- rep(NA_character_, n)
  if (!"Event"  %in% names(out)) out[["Event"]]  <- rep(NA_character_, n)
  
  p1o <- if ("Player_1" %in% names(out)) as.character(out[["Player_1"]]) else rep(NA_character_, n)
  p2o <- if ("Player_2" %in% names(out)) as.character(out[["Player_2"]]) else rep(NA_character_, n)
  
  eto   <- if ("Event_Type" %in% names(out)) tolower(trimws(dplyr::coalesce(as.character(out[["Event_Type"]]), ""))) else rep("", n)
  ero   <- if ("Event_Result" %in% names(out)) tolower(trimws(dplyr::coalesce(as.character(out[["Event_Result"]]), ""))) else rep("", n)
  desco <- if ("Event_Description" %in% names(out)) tolower(dplyr::coalesce(as.character(out[["Event_Description"]]), "")) else rep("", n)
  
  has_p2o <- !is.na(p2o) & nzchar(trimws(p2o))
  
  is_shot_o <- grepl("\\b(2pt|3pt|ft)\\b", eto) |
    grepl("\\b(2pt|3pt|ft)\\b", ero) |
    grepl("free throw|three|3\\s*pt|two\\-?point|jumper|jump shot|layup|dunk|made|miss", desco)
  
  is_ast_o <- (grepl("\\bassist\\b", eto) | grepl("\\bassist\\b", ero) | (grepl("\\bassist\\b", desco) & has_p2o)) & (!is_shot_o)
  
  Event_vec <- as.character(out[["Event"]])
  if (length(Event_vec) != n) Event_vec <- rep(NA_character_, n)
  Event_vec[is_ast_o] <- "assist"
  out[["Event"]] <- Event_vec
  
  Player_vec <- as.character(out[["Player"]])
  if (length(Player_vec) != n) Player_vec <- rep(NA_character_, n)
  
  Player_vec[is_ast_o] <- dplyr::coalesce(p2o[is_ast_o], NA_character_)
  Player_vec[!is_ast_o & is_shot_o] <- dplyr::coalesce(p1o[!is_ast_o & is_shot_o], NA_character_)
  Player_vec[is.na(Player_vec) | !nzchar(Player_vec)] <- dplyr::coalesce(p1o[is.na(Player_vec) | !nzchar(Player_vec)], NA_character_)
  
  out[["Player"]] <- Player_vec
  out
}


build_season_shift_timeline_bundle_fast <- function(
    base,
    team_name,
    player_raw = NULL,
    filter_mode = c("player", "lineup"),
    combo_players = character(0),
    gap_merge_seconds = 2
) {
  filter_mode <- match.arg(filter_mode)
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  assert_cols <- function(df, cols, where = "data") {
    miss <- setdiff(cols, names(df))
    if (length(miss) > 0) stop(sprintf("%s missing: %s", where, paste(miss, collapse = ", ")), call. = FALSE)
    df
  }
  
  # ---- pull base pieces -------------------------------------------------------
  if (!is.list(base)) stop("base must be a list", call. = FALSE)
  
  shifts_all <- if (!is.null(base$shifts_all)) base$shifts_all else base$shifts
  events_all <- if (!is.null(base$events_index)) base$events_index else base$events
  
  if (!is.data.frame(shifts_all) || nrow(shifts_all) == 0) stop("base shifts has 0 rows", call. = FALSE)
  if (!is.data.frame(events_all)) stop("base events is not a data.frame", call. = FALSE)
  
  # Required by plotly build
  shifts_all <- assert_cols(
    shifts_all,
    c("game_id", "game_label", "game_date", "start", "end", "team_pm"),
    where = "bundle$shifts"
  )
  
  shifts_all$game_id <- as.character(shifts_all$game_id)
  shifts_all$start   <- suppressWarnings(as.numeric(shifts_all$start))
  shifts_all$end     <- suppressWarnings(as.numeric(shifts_all$end))
  
  # normalize events (FULL stream for score lookup + lineup stream)
  events <- events_all
  if (is.data.frame(events) && nrow(events) > 0) {
    if ("game_id" %in% names(events)) events$game_id <- as.character(events$game_id)
    if ("Game_Seconds" %in% names(events)) events$Game_Seconds <- suppressWarnings(as.numeric(events$Game_Seconds))
    if ("Event_Team" %in% names(events)) events$Event_Team <- as.character(events$Event_Team)
    if ("Home" %in% names(events)) events$Home <- as.character(events$Home)
    if ("Away" %in% names(events)) events$Away <- as.character(events$Away)
    
    if ("Home_Score" %in% names(events)) events$Home_Score <- suppressWarnings(as.numeric(events$Home_Score))
    if ("Away_Score" %in% names(events)) events$Away_Score <- suppressWarnings(as.numeric(events$Away_Score))
    
    if ("Player_1" %in% names(events)) events$Player_1 <- standardize_player_code(events$Player_1)
    if ("Player_2" %in% names(events)) events$Player_2 <- standardize_player_code(events$Player_2)
    
    lineup_cols <- intersect(c(paste0("Home.", 1:5), paste0("Away.", 1:5)), names(events))
    for (cc in lineup_cols) events[[cc]] <- standardize_player_code(events[[cc]])
  }
  
  empty_bundle <- function() {
    list(
      shifts = shifts_all[0, , drop = FALSE],
      events = if (is.data.frame(events)) events[0, , drop = FALSE] else data.frame()
    )
  }
  
  # ---- helpers ----------------------------------------------------------------
  filter_events_to_team_only <- function(ev, team) {
    if (!is.data.frame(ev) || nrow(ev) == 0) return(ev[0, , drop = FALSE])
    if (!"Event_Team" %in% names(ev)) return(ev)
    team <- as.character(team)
    if (is.null(team) || !nzchar(team)) return(ev[0, , drop = FALSE])
    ev[!is.na(ev$Event_Team) & ev$Event_Team == team, , drop = FALSE]
  }
  
  window_events_to_intervals <- function(ev, intervals) {
    if (!is.data.frame(ev) || nrow(ev) == 0) return(ev[0, , drop = FALSE])
    if (!is.data.frame(intervals) || nrow(intervals) == 0) return(ev[0, , drop = FALSE])
    if (!all(c("game_id", "Game_Seconds") %in% names(ev))) return(ev[0, , drop = FALSE])
    
    ev <- ev[is.finite(ev$Game_Seconds), , drop = FALSE]
    if (nrow(ev) == 0) return(ev)
    
    keep <- rep(FALSE, nrow(ev))
    ev_idx_by_gid <- split(seq_len(nrow(ev)), ev$game_id)
    int_by_gid <- split(intervals, intervals$game_id)
    
    for (gid in intersect(names(ev_idx_by_gid), names(int_by_gid))) {
      idx <- ev_idx_by_gid[[gid]]
      evg <- ev[idx, , drop = FALSE]
      ints <- int_by_gid[[gid]]
      if (nrow(evg) == 0 || nrow(ints) == 0) next
      
      sec <- evg$Game_Seconds
      ok <- rep(FALSE, length(sec))
      for (k in seq_len(nrow(ints))) {
        s0 <- ints$start[k]; e0 <- ints$end[k]
        if (!is.finite(s0) || !is.finite(e0)) next
        ok <- ok | (sec >= s0 & sec <= e0)
      }
      keep[idx] <- ok
    }
    
    ev[keep, , drop = FALSE]
  }
  
  filter_events_for_player <- function(ev, player_code) {
    if (!is.data.frame(ev) || nrow(ev) == 0) return(ev)
    pc <- standardize_player_code(player_code)
    if (is.null(pc) || !nzchar(pc)) return(ev[0, , drop = FALSE])
    
    has_p1 <- "Player_1" %in% names(ev)
    has_p2 <- "Player_2" %in% names(ev)
    if (!has_p1 && !has_p2) return(ev[0, , drop = FALSE])
    
    p1 <- if (has_p1) ev$Player_1 else rep(NA_character_, nrow(ev))
    p2 <- if (has_p2) ev$Player_2 else rep(NA_character_, nrow(ev))
    
    et   <- if ("Event_Type" %in% names(ev)) tolower(trimws(dplyr::coalesce(as.character(ev[["Event_Type"]]), ""))) else rep("", nrow(ev))
    er   <- if ("Event_Result" %in% names(ev)) tolower(trimws(dplyr::coalesce(as.character(ev[["Event_Result"]]), ""))) else rep("", nrow(ev))
    desc <- if ("Event_Description" %in% names(ev)) tolower(dplyr::coalesce(as.character(ev[["Event_Description"]]), "")) else rep("", nrow(ev))
    
    has_p2v <- !is.na(p2) & nzchar(trimws(p2))
    
    is_shot_row <- grepl("\\b(2pt|3pt|ft)\\b", et) |
      grepl("\\b(2pt|3pt|ft)\\b", er) |
      grepl("free throw|three|3\\s*pt|two\\-?point|jumper|jump shot|layup|dunk|made|miss", desc)
    
    # Assist rows: only when player is the assister (Player_2) and not a shot row.
    is_ast_row <- (grepl("\\bassist\\b", et) | grepl("\\bassist\\b", er) |
                     (grepl("\\bassist\\b", desc) & has_p2v)) & (!is_shot_row)
    
    keep <- dplyr::case_when(
      is_ast_row  ~ (p2 == pc),  # assister only
      is_shot_row ~ (p1 == pc),  # shooter only
      TRUE        ~ (p1 == pc)   # other events: treat Player_1 as actor
    )
    
    ev[keep, , drop = FALSE]
  }
  
  filter_events_for_combo_players <- function(ev, combo_codes) {
    if (!is.data.frame(ev) || nrow(ev) == 0) return(ev)
    combo_codes <- standardize_player_code(combo_codes)
    combo_codes <- combo_codes[!is.na(combo_codes) & nzchar(trimws(combo_codes))]
    if (length(combo_codes) == 0) return(ev[0, , drop = FALSE])
    
    has_p1 <- "Player_1" %in% names(ev)
    has_p2 <- "Player_2" %in% names(ev)
    if (!has_p1 && !has_p2) return(ev[0, , drop = FALSE])
    
    p1 <- if (has_p1) ev$Player_1 else rep(NA_character_, nrow(ev))
    p2 <- if (has_p2) ev$Player_2 else rep(NA_character_, nrow(ev))
    
    et   <- if ("Event_Type" %in% names(ev)) tolower(trimws(dplyr::coalesce(as.character(ev[["Event_Type"]]), ""))) else rep("", nrow(ev))
    er   <- if ("Event_Result" %in% names(ev)) tolower(trimws(dplyr::coalesce(as.character(ev[["Event_Result"]]), ""))) else rep("", nrow(ev))
    desc <- if ("Event_Description" %in% names(ev)) tolower(dplyr::coalesce(as.character(ev[["Event_Description"]]), "")) else rep("", nrow(ev))
    
    has_p2v <- !is.na(p2) & nzchar(trimws(p2))
    
    is_shot_row <- grepl("\\b(2pt|3pt|ft)\\b", et) |
      grepl("\\b(2pt|3pt|ft)\\b", er) |
      grepl("free throw|three|3\\s*pt|two\\-?point|jumper|jump shot|layup|dunk|made|miss", desc)
    
    # Assist rows: only when Player_2 is in the combo and not a shot row.
    is_ast_row <- (grepl("\\bassist\\b", et) | grepl("\\bassist\\b", er) |
                     (grepl("\\bassist\\b", desc) & has_p2v)) & (!is_shot_row)
    
    keep <- dplyr::case_when(
      is_ast_row  ~ (p2 %in% combo_codes),  # assister in combo
      is_shot_row ~ (p1 %in% combo_codes),  # shooter in combo
      TRUE        ~ (p1 %in% combo_codes)   # other events: actor (Player_1) in combo
    )
    
    ev[keep, , drop = FALSE]
  }
  
  # ---- score lookup (FULL pbp stream) -----------------------------------------
  # Returns a function score_at(gid, seconds_vec) -> list(team_diff = ..., home = ..., away = ...)
  build_score_at_fn <- function(ev_full, team) {
    ev_full <- assert_cols(ev_full, c("game_id", "Game_Seconds", "Home", "Away", "Home_Score", "Away_Score"), where = "events (score)")
    team <- as.character(team)
    if (is.null(team) || !nzchar(team)) stop("team_name must be provided", call. = FALSE)
    
    ev_full <- ev_full[is.finite(ev_full$Game_Seconds), , drop = FALSE]
    if (nrow(ev_full) == 0) {
      return(function(gid, seconds_vec) {
        n <- length(seconds_vec)
        list(team_diff = rep(NA_real_, n), home = rep(NA_real_, n), away = rep(NA_real_, n))
      })
    }
    
    ev_full <- ev_full[order(ev_full$game_id, ev_full$Game_Seconds), , drop = FALSE]
    split_idx <- split(seq_len(nrow(ev_full)), ev_full$game_id)
    
    # precompute per-game vectors
    per_game <- lapply(names(split_idx), function(gid) {
      idx <- split_idx[[gid]]
      g <- ev_full[idx, , drop = FALSE]
      g <- g[order(g$Game_Seconds), , drop = FALSE]
      
      home_team <- g$Home[which(!is.na(g$Home))[1] %||% 1]
      away_team <- g$Away[which(!is.na(g$Away))[1] %||% 1]
      
      list(
        gid = gid,
        sec = g$Game_Seconds,
        hs  = g$Home_Score,
        as  = g$Away_Score,
        is_home = isTRUE(!is.na(home_team) && home_team == team),
        is_away = isTRUE(!is.na(away_team) && away_team == team)
      )
    })
    names(per_game) <- vapply(per_game, `[[`, "", "gid")
    
    score_at <- function(gid, seconds_vec) {
      gid <- as.character(gid)
      g <- per_game[[gid]]
      n <- length(seconds_vec)
      if (is.null(g) || n == 0) {
        return(list(team_diff = rep(NA_real_, n), home = rep(NA_real_, n), away = rep(NA_real_, n)))
      }
      
      sec0 <- g$sec
      hs0  <- g$hs
      as0  <- g$as
      
      # findInterval gives last index where sec0 <= t
      idx <- findInterval(seconds_vec, sec0, rightmost.closed = TRUE, all.inside = FALSE)
      hs <- rep(NA_real_, n)
      as <- rep(NA_real_, n)
      
      ok <- idx > 0
      hs[ok] <- hs0[idx[ok]]
      as[ok] <- as0[idx[ok]]
      
      # if time precedes first event, treat as 0-0 (common) if we can
      hs[!ok] <- 0
      as[!ok] <- 0
      
      if (isTRUE(g$is_home) && !isTRUE(g$is_away)) {
        team_diff <- hs - as
      } else if (isTRUE(g$is_away) && !isTRUE(g$is_home)) {
        team_diff <- as - hs
      } else {
        team_diff <- rep(NA_real_, n)
      }
      
      list(team_diff = team_diff, home = hs, away = as)
    }
    
    score_at
  }
  
  score_at <- build_score_at_fn(events, team_name)
  
  # ---- derive exact combo intervals from lineup stream -------------------------
  build_combo_intervals_from_events <- function(ev_team, team, combo_codes, gap_merge_seconds = 2) {
    if (!is.data.frame(ev_team) || nrow(ev_team) == 0) return(ev_team[0, , drop = FALSE])
    
    need <- c("game_id", "Game_Seconds", "Home", "Away")
    ev_team <- assert_cols(ev_team, need, where = "events (combo)")
    lineup_need_home <- paste0("Home.", 1:5)
    lineup_need_away <- paste0("Away.", 1:5)
    if (!all(c(lineup_need_home, lineup_need_away) %in% names(ev_team))) {
      stop("events missing some lineup cols Home.1..Home.5/Away.1..Away.5", call. = FALSE)
    }
    
    combo_codes <- standardize_player_code(combo_codes)
    combo_codes <- combo_codes[!is.na(combo_codes) & nzchar(trimws(combo_codes))]
    if (!(length(combo_codes) %in% c(2, 5))) return(ev_team[0, , drop = FALSE])
    
    ev_team <- ev_team[is.finite(ev_team$Game_Seconds), , drop = FALSE]
    if (nrow(ev_team) == 0) return(ev_team[0, , drop = FALSE])
    
    ev_team <- ev_team[order(ev_team$game_id, ev_team$Game_Seconds), , drop = FALSE]
    
    out_list <- list()
    gids <- unique(ev_team$game_id)
    
    for (gid in gids) {
      evg <- ev_team[ev_team$game_id == gid, , drop = FALSE]
      if (nrow(evg) == 0) next
      
      is_home_team <- any(!is.na(evg$Home) & evg$Home == team)
      is_away_team <- any(!is.na(evg$Away) & evg$Away == team)
      
      if (is_home_team && !is_away_team) {
        mat <- as.matrix(evg[, lineup_need_home, drop = FALSE])
      } else if (is_away_team && !is_home_team) {
        mat <- as.matrix(evg[, lineup_need_away, drop = FALSE])
      } else {
        next
      }
      
      on <- apply(mat, 1, function(r) all(combo_codes %in% r))
      if (!any(on, na.rm = TRUE)) next
      
      sec <- evg$Game_Seconds
      
      rle_on <- rle(on)
      ends <- cumsum(rle_on$lengths)
      starts <- ends - rle_on$lengths + 1
      
      seg_idx <- which(rle_on$values %in% TRUE)
      if (length(seg_idx) == 0) next
      
      segs <- lapply(seg_idx, function(j) {
        i0 <- starts[j]
        i1 <- ends[j]
        s0 <- sec[i0]
        e0 <- sec[i1]
        c(start = s0, end = e0)
      })
      segs <- do.call(rbind, segs)
      segs <- as.data.frame(segs, stringsAsFactors = FALSE)
      
      # merge tiny gaps
      if (nrow(segs) > 1 && is.finite(gap_merge_seconds) && gap_merge_seconds >= 0) {
        segs <- segs[order(segs$start, segs$end), , drop = FALSE]
        merged <- list()
        cur_s <- segs$start[1]
        cur_e <- segs$end[1]
        for (k in 2:nrow(segs)) {
          s1 <- segs$start[k]; e1 <- segs$end[k]
          if (is.finite(s1) && is.finite(cur_e) && (s1 - cur_e) <= gap_merge_seconds) {
            cur_e <- max(cur_e, e1, na.rm = TRUE)
          } else {
            merged[[length(merged) + 1]] <- c(start = cur_s, end = cur_e)
            cur_s <- s1; cur_e <- e1
          }
        }
        merged[[length(merged) + 1]] <- c(start = cur_s, end = cur_e)
        segs <- as.data.frame(do.call(rbind, merged), stringsAsFactors = FALSE)
      }
      
      segs$game_id <- gid
      out_list[[gid]] <- segs
    }
    
    if (length(out_list) == 0) return(ev_team[0, , drop = FALSE])
    
    combo_int <- dplyr::bind_rows(out_list)
    combo_int$game_id <- as.character(combo_int$game_id)
    combo_int$start <- suppressWarnings(as.numeric(combo_int$start))
    combo_int$end   <- suppressWarnings(as.numeric(combo_int$end))
    
    # attach game_label/game_date from shifts_all
    meta <- shifts_all %>%
      dplyr::select(game_id, game_label, game_date) %>%
      dplyr::distinct()
    
    combo_int <- combo_int %>%
      dplyr::left_join(meta, by = "game_id")
    
    # compute team_pm per interval using score diff at start/end
    # team_pm := (team_diff_end - team_diff_start)
    if (nrow(combo_int) > 0) {
      combo_int$team_pm <- NA_real_
      by_gid <- split(seq_len(nrow(combo_int)), combo_int$game_id)
      for (gid in names(by_gid)) {
        idx <- by_gid[[gid]]
        s0 <- combo_int$start[idx]
        e0 <- combo_int$end[idx]
        a0 <- score_at(gid, s0)$team_diff
        a1 <- score_at(gid, e0)$team_diff
        combo_int$team_pm[idx] <- suppressWarnings(as.numeric(a1) - suppressWarnings(as.numeric(a0)))
      }
    } else {
      combo_int$team_pm <- NA_real_
    }
    
    combo_int <- combo_int %>%
      dplyr::select(game_id, game_label, game_date, start, end, team_pm, dplyr::everything())
    
    combo_int
  }
  
  # ---- mode logic --------------------------------------------------------------
  mode <- filter_mode %||% "player"
  if (!mode %in% c("player", "lineup")) mode <- "player"
  
  team_name <- as.character(team_name)
  if (is.null(team_name) || !nzchar(team_name)) stop("team_name must be provided", call. = FALSE)
  
  combo_players <- as.character(combo_players)
  combo_players <- combo_players[!is.na(combo_players) & nzchar(trimws(combo_players))]
  combo_players <- standardize_player_code(combo_players)
  
  # team-only events for plotting (prevents opponent events)
  ev_team <- filter_events_to_team_only(events, team_name)
  
  # PLAYER MODE:
  if (identical(mode, "player")) {
    if (is.null(player_raw) || !nzchar(player_raw)) return(empty_bundle())
    
    sh2 <- filter_shift_rows_for_player(shifts_all, player_raw)
    if (!is.data.frame(sh2) || nrow(sh2) == 0) return(empty_bundle())
    
    ev2 <- filter_events_for_player(ev_team, player_raw)
    ev2 <- window_events_to_intervals(ev2, sh2)
    
    return(list(shifts = sh2, events = ev2))
  }
  
  # LINEUP MODE:
  if (identical(mode, "lineup")) {
    if (!(length(combo_players) %in% c(2, 5))) return(empty_bundle())
    
    sh2 <- build_combo_intervals_from_events(
      ev_team = ev_team,
      team = team_name,
      combo_codes = combo_players,
      gap_merge_seconds = gap_merge_seconds
    )
    if (!is.data.frame(sh2) || nrow(sh2) == 0) {
      return(list(shifts = shifts_all[0, , drop = FALSE], events = ev_team[0, , drop = FALSE]))
    }
    
    ev2 <- window_events_to_intervals(ev_team, sh2)
    ev2 <- filter_events_for_combo_players(ev2, combo_players)
    
    return(list(shifts = sh2, events = ev2))
  }
  
  empty_bundle()
}


# ==============================================================================
# OPTIONAL: if generate_shift_data_one_game() is slow, this obviates the
# bigballR::get_player_stats() call by using lineup columns already in pbp.
# Replace the "scraped_stats" section in generate_shift_data_one_game with this.
# ==============================================================================
team_players_from_lineup_cols <- function(pbp, team_prefix) {
  cols <- paste0(team_prefix, ".", 1:5)
  cols <- cols[cols %in% names(pbp)]
  if (length(cols) == 0) return(character(0))
  players <- pbp %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    unlist(use.names = FALSE)
  players <- as.character(players)
  players <- players[!is.na(players) & nzchar(trimws(players))]
  sort(unique(norm_name(players)))
  
  message("team players from lineup cols")
  # glimpse(players)
}



build_season_shift_timeline_data <- function(
    player_raw,
    season,
    team_name,
    sched_df,
    teamids,
    pbp_dir = NULL,
    pbp_loader_many = NULL,            # REQUIRED: function(game_ids, season, pbp_dir, verbose) -> data.frame
    pbp_standardizer = NULL,           # optional: function(pbp_all) -> pbp_all
    shift_builder_one_game = NULL,     # REQUIRED: function(game_id, team_name, teamids, season, pbp) -> shift_data
    events_builder_one_game = NULL,    # REQUIRED: function(pbp, team_name, player_raw, verbose) -> tibble/data.frame
    game_ids = NULL,                   # optional restriction vector
    attach_boxscores = FALSE,
    verbose = FALSE
) {
  
  nzchar_vec <- function(x) !is.na(x) & nzchar(as.character(x))
  
  ensure_cols <- function(df, cols, fill = NA) {
    for (cc in cols) if (!cc %in% names(df)) df[[cc]] <- fill
    df
  }
  
  # ---- validate inputs -------------------------------------------------------
  stopifnot(is.data.frame(sched_df))
  
  if (!("Game_ID" %in% names(sched_df)) && !("game_id" %in% names(sched_df))) {
    stop("sched_df must contain Game_ID or game_id")
  }
  if (!("Date" %in% names(sched_df)) && !("game_date" %in% names(sched_df))) {
    stop("sched_df must contain Date or game_date")
  }
  
  if (is.null(pbp_dir) || !nzchar(as.character(pbp_dir))) pbp_dir <- file.path("cache", "pbp")
  
  if (!is.function(pbp_loader_many))         stop("pbp_loader_many must be a function")
  if (!is.function(shift_builder_one_game))  stop("shift_builder_one_game must be a function")
  if (!is.function(events_builder_one_game)) stop("events_builder_one_game must be a function")
  
  # ---- normalize schedule ----------------------------------------------------
  sched2 <- sched_df
  
  if (!"game_id" %in% names(sched2)) {
    sched2$game_id <- as.character(sched2$Game_ID)
  } else {
    sched2$game_id <- as.character(sched2$game_id)
  }
  
  if (!"game_date" %in% names(sched2)) {
    sched2$game_date <- coerce_to_date(sched2$Date)
  } else {
    sched2$game_date <- coerce_to_date(sched2$game_date)
  }
  
  sched2$game_lbl <- if ("label" %in% names(sched2)) as.character(sched2$label) else NA_character_
  
  if (!"final_score" %in% names(sched2)) {
    sched2$final_score <- dplyr::case_when(
      "Final_Score" %in% names(sched2) ~ as.character(sched2$Final_Score),
      "final"       %in% names(sched2) ~ as.character(sched2$final),
      "Score"       %in% names(sched2) ~ as.character(sched2$Score),
      "score"       %in% names(sched2) ~ as.character(sched2$score),
      TRUE ~ NA_character_
    )
  }
  
  sched2 <- sched2 %>%
    dplyr::filter(nzchar_vec(.data$game_id)) %>%
    dplyr::arrange(.data$game_date, .data$game_id)
  
  if (nrow(sched2) == 0) {
    return(list(shifts = tibble::tibble(), events = tibble::tibble()))
  }
  
  if (!is.null(game_ids) && length(game_ids) > 0) {
    gids <- as.character(game_ids)
    gids <- gids[nzchar_vec(gids)]
    sched2 <- sched2 %>% dplyr::filter(.data$game_id %in% gids)
    if (nrow(sched2) == 0) {
      if (isTRUE(verbose)) message("[timeline] schedule filtered to 0 games after game_ids restriction")
      return(list(shifts = tibble::tibble(), events = tibble::tibble()))
    }
  }
  
  # ---- bulk load pbp ---------------------------------------------------------
  pbp_all <- tryCatch(
    pbp_loader_many(
      game_ids = sched2$game_id,
      season   = season,
      pbp_dir  = pbp_dir,
      verbose  = TRUE
    ),
    error = function(e) {
      message("[timeline] bulk load ERROR: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.function(pbp_standardizer)) {
    pbp_all <- tryCatch(pbp_standardizer(pbp_all), error = function(e) pbp_all)
  }
  
  if (is.null(pbp_all) || !is.data.frame(pbp_all) || nrow(pbp_all) == 0) {
    if (isTRUE(verbose)) message("[timeline] bulk pbp: MISSING/EMPTY")
    return(list(shifts = tibble::tibble(), events = tibble::tibble()))
  }
  
  gid_col <- intersect(c("ID", "Game_ID", "game_id", "GameId", "gameId"), names(pbp_all))[1]
  if (is.na(gid_col) || !nzchar(gid_col)) stop("PBP missing a game id column after bulk load")
  
  # ---- per-game extract ------------------------------------------------------
  get_one_game <- function(gid) {
    
    pbp <- pbp_all %>%
      dplyr::filter(.data[[gid_col]] == as.character(gid)) %>%
      dplyr::mutate(pbp_row_id = dplyr::row_number())   # <<<<<< add this
    
    if (isTRUE(verbose)) {
      message(
        "[timeline] gid=", gid,
        " | pbp rows=", nrow(pbp),
        " | Player_2 nonempty=", sum(nzchar0(pbp$Player_2), na.rm = TRUE),
        " | Event_Description has 'assist'=",
        sum(stringr::str_detect(
          stringr::str_to_lower(dplyr::coalesce(pbp$Event_Description, "")),
          "assist"
        ), na.rm = TRUE)
      )
    }
    
    if (!is.data.frame(pbp) || nrow(pbp) == 0) {
      if (isTRUE(verbose)) message("[timeline] gid=", gid, " | pbp: MISSING/EMPTY (from bulk)")
      return(NULL)
    }
    
    sd <- tryCatch(
      shift_builder_one_game(
        game_id   = gid,
        team_name = team_name,
        teamids   = teamids,
        season    = season,
        pbp       = pbp
      ),
      error = function(e) {
        message("[timeline] gid=", gid, " | shift builder ERROR: ", conditionMessage(e))
        NULL
      }
    )
    
    if (!is.data.frame(sd) || nrow(sd) == 0) {
      if (isTRUE(verbose)) message("[timeline] gid=", gid, " | shift_data: MISSING/EMPTY")
      return(NULL)
    }
    if (!"CleanName" %in% names(sd)) {
      if (isTRUE(verbose)) message("[timeline] gid=", gid, " | shift_data missing CleanName")
      return(NULL)
    }
    
    keep <- match_player_in_shift_data_key(sd, player_raw, verbose = FALSE)
    if (!is.data.frame(keep) || nrow(keep) == 0) {
      if (isTRUE(verbose)) message("[timeline] gid=", gid, " | player not found in shift_data")
      return(NULL)
    }
    
    ev <- tryCatch(
      events_builder_one_game(pbp, team_name, player_raw, verbose = FALSE),
      error = function(e) tibble::tibble()
    )
    
    if (is.data.frame(ev) && nrow(ev) > 0) ev$game_id <- as.character(gid)
    
    # IMPORTANT: use clock_to_sec() so times like "9:51" never become NA
    shifts <- keep %>%
      dplyr::transmute(
        game_id = as.character(gid),
        start_raw = clock_to_sec(.data$start),
        end_raw   = clock_to_sec(.data$end),
        start     = pmin(.data$start_raw, .data$end_raw, na.rm = TRUE),
        end       = pmax(.data$start_raw, .data$end_raw, na.rm = TRUE),
        team_pm   = suppressWarnings(as.numeric(.data$team_pm))
      ) %>%
      dplyr::filter(is.finite(.data$start), is.finite(.data$end), .data$end >= .data$start)
    
    
    if (!is.data.frame(shifts) || nrow(shifts) == 0) {
      if (isTRUE(verbose)) message("[timeline] gid=", gid, " | shifts empty after clock_to_sec()")
      return(NULL)
    }
    
    list(shifts = shifts, events = ev)
  }
  
  out <- purrr::map(sched2$game_id, get_one_game)
  out <- out[!vapply(out, is.null, logical(1))]
  
  shifts_all <- purrr::map_dfr(out, "shifts")
  events_all <- purrr::map_dfr(out, "events")
  events_all <- augment_events_all_with_pbp_context(events_all, pbp_all, gid_col)
  events_all <- normalize_event_context_cols(events_all)
  
  message("events_all")
  # glimpse(events_all)
  
  if (!is.data.frame(shifts_all) || nrow(shifts_all) == 0) {
    return(list(shifts = tibble::tibble(), events = tibble::tibble()))
  }
  
  # ---- attach schedule labels/dates ------------------------------------------
  if (!"Date_chr" %in% names(sched2)) {
    sched2$Date_chr <- if ("Date" %in% names(sched2)) as.character(sched2$Date) else NA_character_
  }
  
  sched_join <- sched2 %>%
    dplyr::select(game_id, game_date, Date = Date_chr, game_lbl, final_score)
  
  shifts_all <- shifts_all %>%
    dplyr::left_join(sched_join, by = "game_id") %>%
    dplyr::mutate(
      game_date = dplyr::coalesce(.data$game_date, coerce_to_date(.data$Date)),
      game_label = dplyr::if_else(
        !is.na(.data$game_lbl) & nzchar_vec(.data$game_lbl),
        .data$game_lbl,
        dplyr::if_else(!is.na(.data$game_date), format(.data$game_date, "%Y-%m-%d"), .data$game_id)
      )
    ) %>%
    dplyr::arrange(.data$game_date, .data$game_id, .data$start)
  
  if (is.data.frame(events_all) && nrow(events_all) > 0) {
    events_all <- events_all %>%
      dplyr::left_join(
        shifts_all %>% dplyr::distinct(.data$game_id, .data$game_date, .data$game_label),
        by = "game_id"
      )
  }
  
  # ---- optional: attach boxscores stats --------------------------------------
  stat_cols <- c("PTS","REB","AST","TOV","STL","BLK")
  
  if (isTRUE(attach_boxscores) && exists("get_player_boxscore_by_game", mode = "function")) {
    bs_game <- tryCatch(
      get_player_boxscore_by_game(
        game_ids   = shifts_all$game_id,
        team_name  = team_name,
        player_raw = player_raw,
        verbose    = verbose
      ),
      error = function(e) {
        message("[timeline] boxscore lookup ERROR: ", conditionMessage(e))
        tibble::tibble()
      }
    )
    
    if (is.data.frame(bs_game) && nrow(bs_game) > 0) {
      shifts_all <- shifts_all %>% dplyr::left_join(bs_game, by = "game_id")
    }
  }
  
  shifts_all <- ensure_cols(shifts_all, stat_cols, fill = NA_integer_)
  
  list(shifts = shifts_all, events = events_all)
}




# ------------------------------------------------------------------------------
# build_season_shift_timeline_plotly()
#
# Drop this into plotly_helpers.R (or wherever your other plotly helpers live).
# This version:
#   - does NOT stop() if PTS/REB/... are missing (it adds NA columns)
#   - can show opponent label (if sched provided with Game_ID + Opponent)
#   - expects shifts_all to be ONE ROW PER GAME for the selected player
#     (if you still have one row per shift-stint, aggregate before calling)
#
# Required columns in shifts_all (minimum):
#   game_id
# Recommended:
#   Date (or join from sched), plus_minus (or PM), minutes (or seconds)
# Optional boxscore columns (added if missing):
#   PTS, REB, AST, TOV, STL, BLK
# ------------------------------------------------------------------------------

# plotly_helpers.R
# ------------------------------------------------------------------------------
# Fix: your server is calling build_season_shift_timeline_plotly(..., pm_min=, pm_max=)
# but the function signature you pasted doesn't accept those args.
# Add them to the signature (and use them to clamp the color range).
# ------------------------------------------------------------------------------



#Add timeline events

# ------------------------------------------------------------------------------
# FIX: shots are being tagged, but they never get plotted because tag_timeline_events()
# currently only detects shots via Event_Type text (free throw / two / three / layup / dunk).
# Your season events table often has shot info in Shot_Value and/or made, but Event_Type is
# generic (or empty), so marker_kind never becomes "SHOT".
#
# Patch tag_timeline_events() to ALSO classify shots when:
#   - Shot_Value is 1/2/3 OR
#   - made is 0/1 AND Event_Description contains "miss"/"made"/"jumper"/"layup"/"3-pt"/etc.
#
# Drop-in replacement: overwrite tag_timeline_events() with this version.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Root cause (season timeline):
# Your tag_timeline_events() is using desc_norm to detect assists:
#   is_ast <- ... | str_detect(desc_norm, "assist")
# But a MADE SHOT description often includes “… assist”, so shots get mis-tagged
# as AST when shot detection fails (or shot_bucket is NA).
#
# Fix:
# 1) Only classify AST from Event/Event_Type (NOT Event_Description).
# 2) Classify shots from Event ("FT"/"2PT"/"3PT") first, then Shot_Value, then text.
# 3) Keep marker_kind precedence: SHOT first, then AST/REB/etc.
#
# Drop-in replacement: overwrite tag_timeline_events() with this version.
# ------------------------------------------------------------------------------

tag_timeline_events <- function(events) {
  if (is.null(events) || !is.data.frame(events) || nrow(events) == 0) return(events)
  
  if (!"Event_Type" %in% names(events)) events$Event_Type <- NA_character_
  if (!"Event"      %in% names(events)) events$Event      <- NA_character_
  if (!"made"       %in% names(events)) events$made       <- NA_integer_
  if (!"Shot_Value" %in% names(events)) events$Shot_Value <- NA_real_
  if (!"Event_Description" %in% names(events)) events$Event_Description <- NA_character_
  if (!"Half_Status" %in% names(events)) events$Half_Status <- NA_character_
  if (!"Time" %in% names(events)) events$Time <- NA_character_
  if (!"Player" %in% names(events)) events$Player <- NA_character_
  
  make_event_hover <- function(player, event, hs, sv, tm, desc) {
    sv_txt <- ifelse(is.na(sv), "", as.character(sv))
    paste0(
      "<b>", player, "</b><br>",
      "<b>Event:</b> ", event, "<br>",
      "<b>Half:</b> ", hs, "<br>",
      "<b>Shot Value:</b> ", sv_txt, "<br>",
      "<b>Time:</b> ", tm,
      ifelse(!is.na(desc) & nzchar(desc), paste0("<br>", desc), "")
    )
  }
  
  ev_txt <- dplyr::coalesce(as.character(events$Event), "")
  et_txt <- dplyr::coalesce(as.character(events$Event_Type), "")
  desc   <- dplyr::coalesce(as.character(events$Event_Description), "")
  
  ev_norm   <- stringr::str_to_lower(ev_txt)
  et_norm   <- stringr::str_to_lower(et_txt)
  desc_norm <- stringr::str_to_lower(desc)
  
  sv <- suppressWarnings(as.numeric(events$Shot_Value))
  
  made_i <- events$made
  if (is.logical(made_i)) made_i <- ifelse(isTRUE(made_i), 1L, 0L)
  made_i <- suppressWarnings(as.integer(made_i))
  made_i[!made_i %in% c(0L, 1L)] <- NA_integer_
  
  # ---- NON-SHOT event classification FIRST (do NOT use desc_norm for assists) ---
  is_ast <- (ev_norm == "assist") | stringr::str_detect(et_norm, "\\bassist\\b")
  is_reb <- (ev_norm == "rebound") | stringr::str_detect(et_norm, "rebound")
  is_tov <- (ev_norm == "turnover")| stringr::str_detect(et_norm, "turnover")
  is_stl <- (ev_norm == "steal")   | stringr::str_detect(et_norm, "\\bsteal\\b")
  is_blk <- (ev_norm == "block")   | stringr::str_detect(et_norm, "block")
  
  # ---- SHOT classification (but never let assist rows become shots) ------------
  shot_bucket_from_event <- dplyr::case_when(
    ev_norm == "ft"  ~ "FT",
    ev_norm == "2pt" ~ "2PT",
    ev_norm == "3pt" ~ "3PT",
    TRUE ~ NA_character_
  )
  
  shot_bucket_from_sv <- dplyr::case_when(
    !is.na(sv) & sv == 1 ~ "FT",
    !is.na(sv) & sv == 2 ~ "2PT",
    !is.na(sv) & sv == 3 ~ "3PT",
    TRUE ~ NA_character_
  )
  
  shot_bucket_from_text <- dplyr::case_when(
    stringr::str_detect(et_norm,   "free throw") ~ "FT",
    stringr::str_detect(et_norm,   "three")      ~ "3PT",
    stringr::str_detect(et_norm,   "two")        ~ "2PT",
    stringr::str_detect(et_norm,   "layup|dunk") ~ "2PT",
    stringr::str_detect(desc_norm, "free throw|ft\\b") ~ "FT",
    stringr::str_detect(desc_norm, "3\\s*pt|three\\-?point|3\\-?pointer") ~ "3PT",
    stringr::str_detect(desc_norm, "two\\-?point|jumper|jump shot|layup|dunk") ~ "2PT",
    TRUE ~ NA_character_
  )
  
  shot_bucket <- dplyr::coalesce(shot_bucket_from_event, shot_bucket_from_sv, shot_bucket_from_text)
  
  looks_like_shot_attempt <- stringr::str_detect(
    desc_norm,
    "made|miss|jumper|jump shot|layup|dunk|tip\\-?in|hook shot|three\\-?point|3\\s*pt|free throw"
  )
  
  # CRITICAL: do not allow assist rows to be treated as shots just because desc contains "made"
  is_shot <- (!is_ast) & (!is.na(shot_bucket) | (!is.na(sv) & sv %in% c(1,2,3)) | looks_like_shot_attempt)
  
  made_infer <- dplyr::case_when(
    !is.na(made_i) ~ made_i,
    stringr::str_detect(desc_norm, "\\bmade\\b") ~ 1L,
    stringr::str_detect(desc_norm, "\\bmiss") ~ 0L,
    TRUE ~ NA_integer_
  )
  
  shot_pts <- dplyr::case_when(
    !is.na(sv) ~ sv,
    shot_bucket == "FT"  ~ 1,
    shot_bucket == "2PT" ~ 2,
    shot_bucket == "3PT" ~ 3,
    TRUE ~ NA_real_
  )
  
  # CRITICAL: precedence—AST first
  marker_kind <- dplyr::case_when(
    is_ast  ~ "AST",
    is_shot ~ "SHOT",
    is_reb  ~ "REB",
    is_tov  ~ "TOV",
    is_stl  ~ "STL",
    is_blk  ~ "BLK",
    TRUE    ~ NA_character_
  )
  
  marker_size <- dplyr::case_when(
    marker_kind == "SHOT" ~ dplyr::case_when(
      shot_pts == 1 ~ 7,
      shot_pts == 2 ~ 10,
      shot_pts == 3 ~ 13,
      TRUE ~ 9
    ),
    marker_kind %in% c("AST","REB","TOV","STL","BLK") ~ 9,
    TRUE ~ NA_real_
  )
  
  marker_symbol <- dplyr::case_when(
    marker_kind == "SHOT" ~ dplyr::if_else(dplyr::coalesce(made_infer, 0L) == 1L, "circle", "circle-open"),
    marker_kind == "AST"  ~ "triangle-up-open",
    marker_kind == "REB"  ~ "diamond",
    marker_kind == "TOV"  ~ "x",
    marker_kind == "STL"  ~ "asterisk",
    marker_kind == "BLK"  ~ "square-open",
    TRUE ~ NA_character_
  )
  
  hover_txt <- make_event_hover(
    player = dplyr::coalesce(events$Player, ""),
    event  = dplyr::case_when(
      marker_kind == "SHOT" ~ paste0(
        shot_bucket,
        ifelse(!is.na(shot_pts), paste0(" (", shot_pts, ")"), ""),
        ifelse(made_infer == 1L, " made", " miss")
      ),
      marker_kind == "AST" ~ "Assist",
      marker_kind == "REB" ~ "Rebound",
      marker_kind == "TOV" ~ "Turnover",
      marker_kind == "STL" ~ "Steal",
      marker_kind == "BLK" ~ "Block",
      TRUE ~ ""
    ),
    hs   = dplyr::coalesce(events$Half_Status, ""),
    sv   = shot_pts,
    tm   = dplyr::coalesce(events$Time, ""),
    desc = events$Event_Description
  )
  
  events %>%
    dplyr::mutate(
      Shot_Value    = sv,
      made          = made_infer,
      shot_bucket   = shot_bucket,
      shot_pts      = shot_pts,
      marker_kind   = marker_kind,
      marker_size   = marker_size,
      marker_symbol = marker_symbol,
      hover_txt     = hover_txt
    )
}


# ------------------------------------------------------------------------------
# FIX for: "Error in if: the condition has length > 1" at build_season_shift_timeline_plotly()
#
# Root cause: somewhere in shift_hover (or related) there is an `if (is.na(final_score))`
# or `if (nzchar(...))` being evaluated on a vector inside with()/paste0().
# Fix: make the hover string with vector-safe ifelse(), never if().
#
# Drop-in replacement: build_season_shift_timeline_plotly() (rows like single-game)
# ------------------------------------------------------------------------------

build_game_row_map <- function(shifts) {
  stopifnot(is.data.frame(shifts))
  need <- c("game_label", "game_date", "game_id")
  miss <- setdiff(need, names(shifts))
  if (length(miss) > 0) stop("build_game_row_map shifts missing: ", paste(miss, collapse = ", "))
  
  lvl <- shifts %>%
    dplyr::distinct(.data$game_label, .data$game_date, .data$game_id) %>%
    dplyr::arrange(.data$game_date, .data$game_id, .data$game_label) %>%
    dplyr::pull(.data$game_label) %>%
    as.character()
  
  tibble::tibble(game_label = lvl, row = seq_along(lvl))
}

# ------------------------------------------------------------------------------
# FIX: server.R calls build_season_shift_timeline_plotly(bundle, team_name=..., pm_min=..., pm_max=...)
# but you changed the signature to require player_raw. Make player_raw OPTIONAL and
# infer it from shifts/events if present, otherwise blank.
#
# Drop-in: replace your current build_season_shift_timeline_plotly() with this.
# ------------------------------------------------------------------------------

build_season_shift_timeline_plotly <- function(bundle,
                                               team_name = NULL,
                                               dark_mode = FALSE,
                                               show_debug = FALSE,
                                               pm_min = -10,
                                               pm_max = 10,
                                               player_raw = NULL,
                                               combo_players = NULL,
                                               filter_mode = c("player","lineup"),
                                               bar_width = 28,
                                               border_width = 2,
                                               y_shot_off = 0.32,
                                               y_misc_off = 0.32,
                                               title_x = 0.02,
                                               margin_l = 140,
                                               margin_r = 80,
                                               margin_t = 80,
                                               margin_b = 60,
                                               tickfont_size = 12,
                                               yaxis_standoff = 0) {
  stopifnot(is.list(bundle), !is.null(bundle$shifts), !is.null(bundle$events))
  filter_mode <- match.arg(filter_mode)
  
  shifts <- bundle$shifts
  events <- bundle$events
  
  # infer player name if not provided
  if (is.null(player_raw) || !nzchar(as.character(player_raw))) {
    cand <- intersect(c("player_raw","player","Player","Name","CleanName"), names(shifts))
    if (length(cand) > 0) {
      v <- unique(na.omit(as.character(shifts[[cand[1]]])))
      if (length(v) > 0 && nzchar(v[1])) player_raw <- v[1]
    } else {
      cand2 <- intersect(c("player_raw","player","Player","Name","CleanName"), names(events))
      if (length(cand2) > 0) {
        v <- unique(na.omit(as.character(events[[cand2[1]]])))
        if (length(v) > 0 && nzchar(v[1])) player_raw <- v[1]
      }
    }
  }
  if (is.null(player_raw) || !nzchar(as.character(player_raw))) player_raw <- ""
  
  need_s <- c("game_id","game_label","game_date","start","end","team_pm")
  miss_s <- setdiff(need_s, names(shifts))
  if (length(miss_s) > 0) stop("bundle$shifts missing: ", paste(miss_s, collapse = ", "))
  
  if (!"final_score" %in% names(shifts)) shifts$final_score <- NA_character_
  
  shifts <- shifts %>%
    dplyr::mutate(
      start   = clock_to_sec(.data$start),
      end     = clock_to_sec(.data$end),
      team_pm = suppressWarnings(as.numeric(.data$team_pm))
    ) %>%
    dplyr::filter(is.finite(.data$start), is.finite(.data$end), .data$end >= .data$start) %>%
    dplyr::mutate(
      team_pm_q = pmax(pmin(round(.data$team_pm), pm_max), pm_min),
      col       = pm_to_color(.data$team_pm_q, pm_min, pm_max)
    )
  
  if (isTRUE(show_debug)) {
    message("[timeline plot] shifts rows=", nrow(shifts),
            " | start finite=", sum(is.finite(shifts$start)),
            " | end finite=", sum(is.finite(shifts$end)))
  }
  
  if (nrow(shifts) == 0) return(plotly::plot_ly())
  
  row_map <- shifts %>%
    dplyr::distinct(.data$game_label, .data$game_date, .data$game_id) %>%
    dplyr::arrange(.data$game_date, .data$game_id, .data$game_label) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::select(game_label, row)
  
  seg <- shifts %>%
    dplyr::left_join(row_map, by = "game_label") %>%
    dplyr::mutate(y = .data$row) %>%
    dplyr::arrange(.data$game_date, .data$game_id, .data$start)
  
  need_e <- c("Game_Seconds","game_id","game_label","game_date")
  miss_e <- setdiff(need_e, names(events))
  if (length(miss_e) > 0) stop("bundle$events missing: ", paste(miss_e, collapse = ", "))
  
  # Harden + tag
  if (!"Event"      %in% names(events)) events$Event      <- NA_character_
  if (!"Event_Type" %in% names(events)) events$Event_Type <- NA_character_
  if (!"Shot_Value" %in% names(events)) events$Shot_Value <- NA_real_
  if (!"made"       %in% names(events)) events$made       <- NA_integer_
  if (!"Event_Description" %in% names(events)) events$Event_Description <- NA_character_
  if (!"Player_2"   %in% names(events)) events$Player_2   <- NA_character_
  if (!"Player"     %in% names(events)) events$Player     <- NA_character_
  if (!"Half_Status" %in% names(events)) events$Half_Status <- NA_character_
  if (!"Time" %in% names(events)) events$Time <- NA_character_
  if (!"Home.1"     %in% names(events)) events$Home.1     <- NA_character_
  if (!"Home.2"     %in% names(events)) events$Home.2     <- NA_character_
  if (!"Home.3"     %in% names(events)) events$Home.3     <- NA_character_
  if (!"Home.4"     %in% names(events)) events$Home.4     <- NA_character_
  if (!"Away.1"     %in% names(events)) events$Away.1     <- NA_character_
  if (!"Away.2"     %in% names(events)) events$Away.2     <- NA_character_
  if (!"Away.3"     %in% names(events)) events$Away.3     <- NA_character_
  if (!"Away.4"     %in% names(events)) events$Away.4     <- NA_character_
  if (!"Away.5"     %in% names(events)) events$Away.5     <- NA_character_
  
  events2 <- events %>%
    dplyr::mutate(
      x          = suppressWarnings(as.numeric(.data$Game_Seconds)),
      Shot_Value = suppressWarnings(as.numeric(.data$Shot_Value)),
      made       = suppressWarnings(as.integer(.data$made)),
      Player_2   = as.character(.data$Player_2),
      Player     = as.character(.data$Player)
    ) %>%
    tag_timeline_events() %>%
    dplyr::filter(!is.na(.data$marker_kind), is.finite(.data$x)) %>%
    dplyr::left_join(row_map, by = "game_label") %>%
    dplyr::filter(!is.na(.data$row)) %>%
    dplyr::mutate(
      y = dplyr::case_when(
        .data$marker_kind == "SHOT" ~ .data$row - y_shot_off,
        TRUE                        ~ .data$row + y_misc_off
      )
    )
  
  # --- FT stacking (kept as-is) ---
  ft_stack_delta <- 0.12
  
  events2 <- events2 %>%
    dplyr::mutate(
      desc_l = stringr::str_to_lower(dplyr::coalesce(.data$Event_Description, "")),
      is_ft_shot = (.data$marker_kind == "SHOT") &
        (dplyr::coalesce(.data$shot_bucket, "") == "FT" | dplyr::coalesce(.data$Shot_Value, NA_real_) == 1)
    ) %>%
    dplyr::mutate(
      ft_n = suppressWarnings(as.integer(stringr::str_match(.data$desc_l, "\\b(\\d)\\s*of\\s*(\\d)\\b")[,2])),
      ft_m = suppressWarnings(as.integer(stringr::str_match(.data$desc_l, "\\b(\\d)\\s*of\\s*(\\d)\\b")[,3]))
    ) %>%
    dplyr::group_by(.data$game_id, .data$row, .data$Player, .data$x) %>%
    dplyr::mutate(
      ft_group_size = dplyr::if_else(any(.data$is_ft_shot), sum(.data$is_ft_shot), 0L),
      ft_stack_idx = dplyr::case_when(
        .data$is_ft_shot & !is.na(.data$ft_n) ~ .data$ft_n,
        .data$is_ft_shot ~ dplyr::row_number(),
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      y = dplyr::if_else(
        .data$is_ft_shot & !is.na(.data$ft_stack_idx),
        .data$y + (.data$ft_stack_idx - 1L) * ft_stack_delta,
        .data$y
      )
    ) %>%
    dplyr::select(-.data$desc_l, -.data$is_ft_shot, -.data$ft_n, -.data$ft_m, -.data$ft_group_size, -.data$ft_stack_idx)
  
  if (isTRUE(show_debug)) {
    message("[timeline plot] seg rows=", nrow(seg),
            " | events2 rows=", nrow(events2),
            " | AST rows=", sum(events2$marker_kind == "AST", na.rm = TRUE))
  }
  
  final_second <- suppressWarnings(max(seg$end, na.rm = TRUE))
  if (!is.finite(final_second)) final_second <- 2400
  
  max_row <- suppressWarnings(max(row_map$row, na.rm = TRUE))
  if (!is.finite(max_row)) max_row <- 1
  
  y_ticks <- row_map$row
  y_text  <- row_map$game_label

  pal <- get_event_palette(dark_mode)
  shot_make_color <- pal$shot_make
  shot_miss_color <- pal$shot_miss
  block_color <- pal$block
  # ADDED: Extract assist color from palette for dark mode visibility
  assist_color <- pal$assist
  
  
  
  seg_text <- tibble::tibble(
    x = (seg$start + seg$end) / 2,
    y = seg$y,
    txt = ifelse(is.na(seg$team_pm), "", sprintf("%+g", seg$team_pm))
  ) %>%
    dplyr::filter(is.finite(.data$x), is.finite(.data$y), nzchar(.data$txt))
  
  # --- dynamic title -----------------------------------------------------------
  combo_label <- NULL
  if (!is.null(combo_players) && length(combo_players) > 0) {
    cp <- as.character(combo_players)
    cp <- cp[!is.na(cp) & nzchar(trimws(cp))]
    if (length(cp) > 0) combo_label <- paste(cp, collapse = " + ")
  }
  
  title_txt <- if (identical(filter_mode, "lineup") && !is.null(combo_label) && nzchar(combo_label)) {
    paste0(combo_label, " — ",
           if (!is.null(team_name) && nzchar(as.character(team_name))) paste0(as.character(team_name), " — ") else "",
           "Season Shift Timeline")
  } else {
    paste0(
      if (nzchar(player_raw)) paste0(player_raw, " — ") else "",
      if (!is.null(team_name) && nzchar(as.character(team_name))) paste0(as.character(team_name), " — ") else "",
      "Season Shift Timeline"
    )
  }
  
  p <- plotly::plot_ly()
  
  # --- SHIFT BARS ---
  seg_by_col <- split(seg, seg$col)
  for (cc in names(seg_by_col)) {
    dfc <- seg_by_col[[cc]]
    p <- p %>%
      plotly::add_segments(
        data = dfc,
        x = ~start, xend = ~end,
        y = ~y,     yend = ~y,
        line = list(width = bar_width, color = cc),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      ) %>%
      plotly::add_segments(
        data = dfc,
        x = ~start, xend = ~end,
        y = ~y,     yend = ~y,
        line = list(color = "gray30", width = border_width),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
  }
  
  # MODIFIED: Increased textfont size from 14 to 18 for better readability
  if (nrow(seg_text) > 0) {
    p <- p %>%
      plotly::add_text(
        data = seg_text,
        x = ~x, y = ~y, text = ~txt,
        textfont = list(size = 18, color = "black", family = "Arial Black"),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
  }
  
  p <- p %>%
    plotly::add_trace(
      type = "scatter", mode = "lines",
      x = c(1200, 1200),
      y = c(0.5, max_row + 0.5),
      line = list(color = "gray50", dash = "dash", width = 2),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  
  shots <- events2 %>% dplyr::filter(.data$marker_kind == "SHOT")
  misc  <- events2 %>% dplyr::filter(.data$marker_kind != "SHOT")
  
  if (nrow(shots) > 0) {
    s_make <- shots %>% dplyr::filter(dplyr::coalesce(.data$made, 0L) == 1L)
    s_miss <- shots %>% dplyr::filter(dplyr::coalesce(.data$made, 0L) != 1L)
    
    if (nrow(s_make) > 0) {
      p <- p %>% plotly::add_markers(
        data = s_make,
        x = ~x, y = ~y,
        marker = list(symbol = "circle", color = shot_make_color, size = s_make$marker_size,
                      line = list(color = shot_make_color, width = 1)),
        text = ~hover_txt, hoverinfo = "text",
        name = "Shot (Make)", showlegend = TRUE,
        inherit = FALSE
      )
    }
    if (nrow(s_miss) > 0) {
      p <- p %>% plotly::add_markers(
        data = s_miss,
        x = ~x, y = ~y,
        marker = list(symbol = "circle-open", color = shot_miss_color, size = s_miss$marker_size,
                      line = list(color = shot_miss_color, width = 2)),
        text = ~hover_txt, hoverinfo = "text",
        name = "Shot (Miss)", showlegend = TRUE,
        inherit = FALSE
      )
    }
  }
  
  # MODIFIED: Assist markers now use palette color for dark mode visibility
  ast <- misc %>% dplyr::filter(.data$marker_kind == "AST")
  if (nrow(ast) > 0) {
    p <- p %>% plotly::add_markers(
      data = ast,
      x = ~x, y = ~y,
      marker = list(
        symbol = "triangle-up-open",
        color  = assist_color,
        size   = dplyr::coalesce(ast$marker_size, 10),
        line   = list(color = assist_color, width = 2)
      ),
      text = ~hover_txt, hoverinfo = "text",
      name = "Assist", showlegend = TRUE,
      inherit = FALSE
    )
  }
  
  add_misc <- function(p, df, kind, symbol, name, color, outline_color = NULL, outline_width = NULL) {
    dd <- df %>% dplyr::filter(.data$marker_kind == kind)
    if (nrow(dd) == 0) return(p)
    
    m <- list(symbol = symbol, color = color, size = 10)
    if (!is.null(outline_color) || !is.null(outline_width)) {
      m$line <- list(
        color = outline_color %||% color,
        width = outline_width %||% 2
      )
    } else {
      m$line <- list(color = color, width = 2)
    }
    
    p %>% plotly::add_markers(
      data = dd,
      x = ~x, y = ~y,
      marker = m,
      text = ~hover_txt, hoverinfo = "text",
      name = name, showlegend = TRUE,
      inherit = FALSE
    )
  }
  
  p <- add_misc(p, misc, "TOV", "x", "Turnover", "red", outline_color = "black", outline_width = 1)
  p <- add_misc(p, misc, "STL", "asterisk", "Steal", "#FFD700")
  p <- add_misc(p, misc, "BLK", "square-open", "Block", block_color)
  p <- add_misc(p, misc, "REB", "diamond", "Rebound", "steelblue")
  
  p %>%
    plotly::layout(
      title = list(text = title_txt, x = title_x, xanchor = "left"),
      xaxis = list(
        range = c(-220, final_second),
        tickvals = c(0, 600, 1200, 1800, 2400),
        ticktext = c("0:00", "10:00", "Halftime", "30:00", "End Regulation"),
        title = ""
      ),
      yaxis = list(
        tickmode = "array",
        tickvals = y_ticks,
        ticktext = y_text,
        autorange = "reversed",
        title = "",
        ticklabelposition = "outside",
        ticks = "",
        ticklen = 0,
        tickfont = list(size = tickfont_size),
        standoff = yaxis_standoff
      ),
      legend = list(
        orientation = "h",
        x = 0.98, xanchor = "right",
        y = 1.12, yanchor = "top"
      ),
      margin = list(l = margin_l, r = margin_r, t = margin_t, b = margin_b),
      hovermode = "closest"
    )
}
