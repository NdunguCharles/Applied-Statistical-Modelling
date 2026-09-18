data_clean <- data_clean %>%
  mutate(
    Predicted_Sales = predict(model),
    Residuals = residuals(model)
  )

#Actual vs predicted
ggplot(
  data_clean,
  aes(x = Global_Sales, y = Predicted_Sales)
) +
  geom_point(size = 3, color = "steelblue") +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Actual vs Predicted Global EV Sales",
    x = "Actual Sales",
    y = "Predicted Sales"
  ) +
  theme_minimal()

model_year <- lm(
  Global_Sales ~ Battery_Price + BEV_Price + Year,
  data = data_clean
)

summary(model_year)

#ANOVA
anova(model, model_year)

AIC(model)
AIC(model_year)
