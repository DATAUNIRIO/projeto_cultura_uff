df <- tibble::tribble(
  ~key, ~value,
  1, "Before",
  1, "After",
  1, "During",
  1, "Before",
  2, "Before",
  2, "After",
  3, "During"
)

df %>%
  dplyr::count(key, value) %>%
  dplyr::group_by(key) %>%
  dplyr::mutate(p = n / sum(n)) %>%
  ggplot() + 
  geom_bar(
    mapping = aes(x = key, y = p, fill = value),
    stat = "identity",
    position = position_dodge()
  ) + 
  scale_y_continuous(labels = scales::percent_format())
