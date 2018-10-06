# Introduction
## Tools Used
The bulk of data analysis was performed in R, with machine learning model developed in MATLAB.

# Analysis
1. Download the dataset from [kaggle.com](https://www.kaggle.com/terminus7/pokemon-challenge/version/1) and extract the zip file to the R working directory.
2. `source()` the `import_data.R` file, which will import the data and generate data necessary to answer specific questions about the data. It will also write the combat features to `combat_features.csv`.
3. `source()` the `questions.R` file, which will import question-answering functions.

# Questions
All questions are functions of the form `qn(...)` with the appropriate number substituted for `n`.
1. _How many unique pokemon ids are there?_
    Prints the value, and returns it. There are 800 unique pokemon ids.
2. _What does the left-side-wins space look like?_
    Generates a heatmap image without clustering. The image shows sparse coverage of the full space.
3. _What fraction of coverage of the space is there in the dataset?_
    Prints the value as a percent, and returns the value as a ratio. The coverage percentage is 7.81%.
4. _How many repeat battles are there?_
    The space of battles is symmetric, so it is possible for {id1,id2,id1} to also appear as {id2,id1,id1}. Prints the value, and returns it. There are 3650 repeat battles, which is 7.3% of all battles.
5. _Which pokemon type is most likely to win?_
    Creates bar charts of win ratio by first type, with one panel for legendary, and one for non-legendary. It appears that of legendary, it is fighting, grass, and normal. Of non-legendary, it is flying, dark, electric. However, the numbers are so close together, it is probable that type does not play a large role in win ratio. Hence it is probably not worth our time to consider types. Furthermore, while legendaries appear to perform better than non-legendaries, this is not because they are legendary, per se, but instead due to their higher attributes. Legendary-ness does not play a direct role in determining performance in the games, and is instead a marker of pokemon that will perform better due to their attributes. As an interesting side note, there are no first-typed poison legendary pokemon.
6. _Which attribute plays the greatest role in winning?_
    Creates scatter plots with panels for each attribute, with linear best-fit models. Speed has the highest correlation with win ratio, followed by attack and special attack. It is worth ensuring that the correlation between speed and performance is legitimate.
7. _Is the speed attribute subject to Simpson's paradox with respect to type?_
    Creates scatter plots with panels for each type, with linear best-fit models. The correlation is strong for every first type.
8. _What is the distribution of wins by attribute difference?_
    Note that difference here means the difference of an attribute between first and second pokemon in the combat data. Creates distribution plots with panels for each attribute, with colors split by left-hand-side win/loss. There is significant overlap for each of the attribute differences, with the exception of speed. With speed, there is a clear separation between wins and losses, with positive difference (first pokemon has higher speed) have more wins than losses, and vice-versa with negative difference.
9. _Is it possible to better model battles using domain knowledge?_
    From the games, damage is proportional to the ratio of attacker attack stat (attack or special attack) and defender defense stat (defense or special defense). Examining the difference of attack stats and their respective defense stats gives us an advantage value. Examining the difference in maximum advantages should correlate better than raw attribute differences. Speed difference win/loss distribution is compared with the same for advantage difference. There does not appear to be any significant improvement in separability. It is likely we should stop here, and settle for a predictive model formed from the types and raw attribute differences.

# Predicting Performance
MATLAB has excellent features for exploring classification learning models, and was used to produce the results below. Training examples take the form of `{first_pokemon_types, second_pokemon_types, attribute_differences, did_first_pokemon_win}`. Every available model was trained using 5-fold validation. The best models appear to be the Fine Tree and Bagged Trees Ensemble models.