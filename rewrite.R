library(tidyverse)
library(splines)
library(furrr)

plan(multiprocess)
availableCores()
options("future.fork.enable" = T)

league.info = read_csv('league_info.csv')

league.info

# download.file("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv", 'spi_matches.csv')

games = read_csv("spi_matches.csv")

games

dif = read_csv("draw_inflation_factors.csv")

dif

games.split = league.info %>% 
  select(league, restart_date) %>% 
  left_join(games) %>% 
  select(-starts_with('importance'), -starts_with('xg'), -starts_with('nsxg'), -starts_with('adj_sc')) %>% 
  group_by(league) %>% 
  nest() %>% 
  mutate(
    pre.covid.data = map(
      data,
      ~.x %>% filter(date < restart_date)
    ),
    post.covid.data = map(
      data,
      ~.x %>%
        filter(date >= restart_date) %>% 
        filter(date <= Sys.Date()) %>% 
        filter(!is.na(score1))
    ),
  )

games.split

# Part I ------------------------------------------------------------------

# how are leagues performing against 538's own predictions?

# each game in the 538 data has a P(win) P(loss) P(tie) that add up to 1
# these probabilities have a baked-in 60% HFA reduction

generate.points.distribution = function(post.covid.lg) {
  # generate a random number between 0 and 1 and find where it falls to "simulate" the outcome of that game
  get_exp_home_points <- function(x) {
    p <- runif(nrow(post.covid.lg))
    # total the home points won in all of the games based on the random number
    # this is one "season"
    case_when(
      p <= post.covid.lg$prob1 ~ 3,
      p <= post.covid.lg$prob1  + post.covid.lg$probtie ~ 1,
      T ~ 0
    ) %>%
      sum()
  }
  
  # redo the season 10,000 times to get an idea of how often home teams should be accruing points
  sims = future_map_dbl(1:10000, get_exp_home_points)
  
  return(sims)
}

eval.after = games.split %>% 
  mutate(
    home.pts = map_dbl(
      post.covid.data,
      ~.x %>% 
        mutate(home.pts = (score1 > score2) * 3 + (score1 == score2) * 1) %>% 
        pull(home.pts) %>% 
        sum()
    ),
    pts.distribution = map(post.covid.data, generate.points.distribution),
    exp.home.pts = map_dbl(pts.distribution, mean),
    sd.home.pts = map_dbl(pts.distribution, sd),
    actual.z = (home.pts - exp.home.pts) / sd.home.pts
  ) %>% 
  select(-ends_with('data')) %>% 
  arrange(actual.z)

eval.after

plot.exp.pred.points = eval.after %>%
  ungroup() %>% 
  mutate(league = fct_inorder(league)) %>% 
  select(league, home.pts, pts.distribution) %>%
  unnest(pts.distribution) %>% 
  ggplot() +
  geom_density(aes(x = pts.distribution)) +
  geom_vline(aes(xintercept = home.pts), color = 'dodgerblue') +
  facet_wrap(. ~ league, scales = 'free_x') +
  theme_minimal() +
  labs(
    x = "",
    y = "Density",
    title = "Distribution of points expected to be won by home teams",
    subtitle = 'Actual points won by home teams',
    caption = "Using 538 SPI data, which has a home-field advantage reduction already factored in"
  ) +
  theme(
    plot.subtitle = element_text(color = 'dodgerblue')
  )

plot.exp.pred.points

ggsave(filename = 'expected-predicted-points-by-league.png', plot = plot.exp.pred.points, width = 12, height = 8)

# Part II -----------------------------------------------------------------

# what would a more appropriate HFA reduction be?

# construct a model to emulate 538's
# this model will take the SPI rating for a particular team, an SPI rating for the opponent and a home-team T/F flag to generate the expected goal total for that team
# once the model can generate expected goals totals (lambdas), those can be used to generate expected goals distributions

make.pre.covid.model = function(pre.covid.games) {
  pre.covid.games = pre.covid.games %>%
    mutate(gameid = 1:nrow(.)) %>%
    select(season, date, gameid, team1, team2, spi1, spi2, proj_score1, proj_score2, score1, score2)
  
  pre.covid.games
  
  # turn wide data into long so each team in the pair is evaluated
  make.longer = function(df, colname) {
    df %>% 
      select(season, date, gameid, starts_with(colname)) %>% 
      pivot_longer(-season:-gameid, names_to = 'teamid', values_to = colname) %>% 
      mutate(teamid = str_sub(teamid, start = -1))
  }
  
  pre.covid.train = make.longer(pre.covid.games, 'team') %>% 
    left_join(make.longer(pre.covid.games, 'spi'), by = c("season", "date", "gameid", "teamid")) %>% 
    left_join(make.longer(pre.covid.games, 'proj_score'), by = c("season", "date", "gameid", "teamid")) %>% 
    left_join(make.longer(pre.covid.games, 'score'), by = c("season", "date", "gameid", "teamid")) 
  
  pre.covid.train = pre.covid.train %>% 
    left_join(
      pre.covid.train %>%
        mutate(teamid = if_else(teamid == '1', '2', '1')) %>%
        rename(team_opp = team, spi_opp = spi, proj_score_opp = proj_score, score_opp = score),
      by = c("season", "date", "gameid", "teamid")
    ) %>% 
    # home team is 1 and away team is negative 1
    mutate(home = if_else(teamid == 1, 1, -1))
  
  pre.covid.train
  
  # fit model
  
  model = lm(log(proj_score) ~ ns(spi, 3) + ns(spi_opp, 3) + home, data = pre.covid.train)
  
  return(model)
}

before.modeled = games.split %>%
  mutate(
    model = map(pre.covid.data, make.pre.covid.model),
    hfa.before = map_dbl(model, ~pluck(coef(.x), 'home'))
  ) %>% 
  arrange(-hfa.before)

before.modeled

# 538 has applied an across the board 60% HFA reduction
# would a different reduction be better?
# it seems league-dependent
# and could there actually be an increase?

# check all possibilities from a total elimination of HFA to a 15% increase in HFA for each league
hfa.adjustment = seq(0, 1.15, 0.05)

after.adjustments = before.modeled %>% 
  select(-data, -pre.covid.data) %>% 
  mutate(hfa.adjustment = map(league, ~hfa.adjustment)) %>% 
  unnest(hfa.adjustment) %>% 
  mutate(hfa.now = hfa.adjustment * hfa.before)

after.adjustments

m = after.adjustments$model[[5]]
m

new.hfa = after.adjustments$hfa.now[[5]]
new.hfa

m$coefficients['home'] <- new.hfa

m

make.projection = function(s1, s2, h, model) {
  predict(model, newdata = tibble(spi = s1, spi_opp = s2, home = h)) %>%
    exp()
}

after.adjustments$post.covid.data[[5]] %>% 
  select(1:8) %>% 
  mutate(
    pred_lambda1 = map2_dbl(
      spi1, spi2,
      ~make.projection(.x, .y, 1, m)
    ),
    pred_lambda2 = map2_dbl(
      spi1, spi2,
      ~make.projection(.y, .x, -1, m)
    )
  ) %>% 
  view()


# old ---------------------------------------------------------------------





pre.covid.preds = pre.covid.games %>% 
  mutate(
    pred_lambda1 = map2_dbl(
      spi1, spi2,
      ~make.projection(.x, .y, 1)
    ),
    pred_lambda2 = map2_dbl(
      spi1, spi2,
      ~make.projection(.y, .x, -1)
    )
  )

pre.covid.preds

# l1 = pre.covid.preds$pred_lambda1[13]
# l2 = pre.covid.preds$pred_lambda2[13]
# 
# make.plot = function(l1, l2) {
#   tibble(
#     goals = 0:10,
#     g_home = dpois(goals, l1),
#     g_away = dpois(goals, l2)
#   ) %>% 
#     pivot_longer(-goals, names_to = 'team', values_to = 'probs') %>% 
#     ggplot(aes(goals, probs, color = team)) +
#     geom_point() +
#     geom_line() +
#     scale_x_continuous(breaks = 0:10) +
#     theme_minimal()
# }
# 
# make.plot(l1, l2)

make.score.matrix = function(l1, l2, dif) {
  max.goals = 10
  goals.dist1 = dpois(0:max.goals, l1)
  goals.dist2 = dpois(0:max.goals, l2)
  score.matrix = goals.dist1 %o% goals.dist2
  sum(score.matrix)
  # adjust for league tie inflation
  score.matrix
  diag(score.matrix) = dif * diag(score.matrix)
  sum(score.matrix)
  # divide all probabilities by new inflated total
  score.matrix = score.matrix / sum(score.matrix)
  sum(score.matrix)
  return(score.matrix)
}

pre.covid.preds = pre.covid.preds %>% 
  mutate(
    score.matrix = map2(pred_lambda1, pred_lambda2, make.score.matrix, dif = league.dif),
    tie.prob = map_dbl(score.matrix, ~sum(diag(.x))),
    home.prob = map_dbl(score.matrix, ~sum(.x[upper.tri(.x)])),
    away.prob = map_dbl(score.matrix, ~sum(.x[lower.tri(.x)])),
  )

pre.covid.preds
