#ARCSIN TRANSFORMATION

final_df$Flight_ASin=asin(sqrt(final_df$Flight/100))
final_df$Bonus_ASin=asin(final_df$Bonus)
