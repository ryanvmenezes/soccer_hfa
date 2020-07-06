library(tidyverse)
library(splines)
library(furrr)
library(ggrepel)

plan(multiprocess)
availableCores()
options("future.fork.enable" = T)

set.seed(10)

league.info = read_csv('league_info.csv')

league.info

# download.file("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv", 'spi_matches.csv')

games = read_csv("spi_matches.csv")

games

dif = read_csv("draw_inflation_factors.csv")

dif

games.split = league.info %>% 
  select(league, restart_date) %>% 
  mutate(restart_date = lubridate::mdy(restart_date)) %>% 
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
  ) %>% 
  ungroup()

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
    post.covid.games = map_int(post.covid.data, nrow),
    home.pts = map_dbl(
      post.covid.data,
      ~.x %>% 
        mutate(home.pts = (score1 > score2) * 3 + (score1 == score2) * 1) %>% 
        pull(home.pts) %>% 
        sum()
    ),
    pts.distribution = future_map(post.covid.data, generate.points.distribution, .progress = TRUE),
    exp.home.pts = map_dbl(pts.distribution, mean),
    result.likelihood = map2_dbl(home.pts, pts.distribution, ~mean(.y < .x)),
    significant = result.likelihood < 0.025 | result.likelihood > 0.975
  ) %>% 
  select(-ends_with('data')) %>% 
  arrange(result.likelihood)

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

eval.after %>% 
  select(-pts.distribution) %>% 
  write_csv('expected-predicted-points-by-league.csv')

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
    model = future_map(pre.covid.data, make.pre.covid.model),
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

all.adjustments = before.modeled %>% 
  select(-data, -pre.covid.data) %>% 
  mutate(hfa.adjustment = map(league, ~hfa.adjustment)) %>% 
  unnest(hfa.adjustment) %>% 
  mutate(hfa.now = hfa.adjustment * hfa.before) %>% 
  # get tie inflations
  left_join(dif %>% select(-league_id))

all.adjustments

predict.new.data = function(data, model, hfa.now, league.dif) {
  make.projection = function(s1, s2, h, model) {
    predict(model, newdata = tibble(spi = s1, spi_opp = s2, home = h)) %>%
      exp()
  }
  
  # with the two lambdas in hand, construct a score matrix
  # each lambda is the expected mean of a poisson distribution
  
  make.score.matrix = function(l1, l2, dif) {
    max.goals = 10
    # construct the distributions
    goals.dist1 = dpois(0:max.goals, l1)
    goals.dist2 = dpois(0:max.goals, l2)
    score.matrix = goals.dist1 %o% goals.dist2
    # adjust for league tie inflation
    diag(score.matrix) = dif * diag(score.matrix)
    # divide all probabilities by new inflated total
    score.matrix = score.matrix / sum(score.matrix)
    return(score.matrix)
  }
  
  model$coefficients['home'] <- hfa.now
  
  data %>% 
    select(season, date, team1, team2, spi1, spi2, score1, score2) %>% 
    mutate(
      # predict goals (lambdas)
      pred_lambda1 = map2_dbl(
        spi1, spi2,
        ~make.projection(.x, .y, 1, model)
      ),
      pred_lambda2 = map2_dbl(
        spi1, spi2,
        ~make.projection(.y, .x, -1, model)
      ),
      # use lambdas for points distribution
      score.matrix = map2(pred_lambda1, pred_lambda2, make.score.matrix, dif = league.dif),
      probtie = map_dbl(score.matrix, ~sum(diag(.x))),
      prob1 = map_dbl(score.matrix, ~sum(.x[lower.tri(.x)])),
      prob2 = map_dbl(score.matrix, ~sum(.x[upper.tri(.x)])),
    )
}

pred.adjustments = all.adjustments %>% 
  mutate(
    new.preds = future_pmap(
      list(post.covid.data, model, hfa.now, tie_inflation),
      predict.new.data,
      .progress = TRUE
    )
  ) %>%
  select(-post.covid.data)

pred.adjustments

sim.adjustments = pred.adjustments %>% 
  ungroup() %>% 
  mutate(
    home.pts = map_dbl(
      new.preds,
      ~.x %>% 
        mutate(home.pts = (score1 > score2) * 3 + (score1 == score2) * 1) %>% 
        pull(home.pts) %>% 
        sum()
    ),
    pts.distribution = future_map(new.preds, generate.points.distribution, .progress = TRUE),
  )

sim.adjustments

eval.adjustments = sim.adjustments %>% 
  select(league, hfa.before, hfa.adjustment, hfa.now, home.pts, pts.distribution) %>% 
  unnest(pts.distribution) %>% 
  group_by(league, hfa.before, hfa.adjustment, hfa.now, home.pts) %>% 
  summarise(
    result.likelihood = mean(pts.distribution <= home.pts),
    mean.pts = mean(pts.distribution),
    min.pts = min(pts.distribution),
    q025.pts = quantile(pts.distribution, 0.025),
    median.pts = median(pts.distribution),
    q975.pts = quantile(pts.distribution, 0.975),
    max.pts = max(pts.distribution),
    sd.pts = sd(pts.distribution),
    .groups = 'drop'
  ) %>% 
  mutate(
    z.pts = (home.pts - mean.pts) / sd.pts
  )

eval.adjustments

eval.adjustments %>% write_csv('hfa-adjustment-results.csv')

# pick the adjustment that is the least unlikely (based on z value)

final.adjustment = eval.adjustments %>% 
  group_by(league) %>% 
  filter(abs(z.pts) == min(abs(z.pts))) %>% 
  arrange(hfa.adjustment) %>% 
  select(league, hfa.before, hfa.adjustment, hfa.now)

final.adjustment

final.adjustment %>% write_csv('final-hfa-adjustment.csv')

plot.before.after.hfa = final.adjustment %>% 
  mutate(
    hfa.before = (exp(hfa.before) - 1) * 100,
    hfa.now = (exp(hfa.now) - 1) * 100,
  ) %>% 
  left_join(league.info %>% select(league, alias)) %>% 
  ggplot(aes(hfa.before, hfa.now, label = alias)) +
  geom_abline(slope = 1, intercept = 0, color = 'maroon') +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(limits = c(0,30)) +
  scale_y_continuous(limits = c(0,30)) +
  theme_minimal() +
  labs(
    title = 'Change in home-field advantage since restart',
    subtitle = 'Before: Estimate of 538\'s SPI HFA factor\nAfter: Reduction in pre-COVID HFA that best matches new data',
    x = 'HFA before',
    y = 'HFA after',
    caption = 'HFA is represented as percentage increase in predicted goals scored while home team'
  )

plot.before.after.hfa

ggsave(filename = 'hfa-comparison-before-after.png', plot = plot.before.after.hfa, width = 12, height = 8)
