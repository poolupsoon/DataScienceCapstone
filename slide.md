Data Science Capstone: Predictive Text Model
========================================================
author: Author: Poo, L. S.
date: Date: 19 June 2017
autosize: true

Overview
========================================================
1. The goal of this Shiny app is to create a data product to highlight the prediction algorithm that I have built and to provide an interface that can be accessed by others.
2. This Shiny app accepts an n-gram phrase entered by the user and predicts the next word.
3. The Shiny app is here: [https://poolupsoon.shinyapps.io/DataScienceCapstone/](https://poolupsoon.shinyapps.io/DataScienceCapstone/)
4. The ui.R and server.R files are here: [https://github.com/poolupsoon/DataScienceCapstone/](https://github.com/poolupsoon/DataScienceCapstone/)
5. The documentation on how to use the Shiny app is here: [https://poolupsoon.shinyapps.io/DataScienceCapstone/_w_f6178dfe/documentation.html](https://poolupsoon.shinyapps.io/DataScienceCapstone/_w_f6178dfe/documentation.html)

Algorithm
========================================================
1. Firstly, the input phrase will be processed and cleaned before a prediction is made.
2. Based on three previous words, the algorithm will look for matching 3-gram tokens in the dictionary.
3. If no matches are found, then based on two previous words, the algorithm will look for matching 2-gram tokens in the dictionary.
4. If no matches are found, then based on one previous word, the algorithm will look for matching 1-gram tokens in the dictionary.
5. If still no matches are found, the algorithm will look for words with highest frequencies in the dictionary.
6. The antonyms for previous three words will be identified and included in the prediction results.

Efficiency and Accuracy
========================================================
1. The size of the predictive model has to be carefully considered. Some accuracy has to be sacrificed in order to have a fast enough and small enough predictive model to load into Shiny.
2. The predictive model requires a size of approximate **54.3 Mb** to **72.6 Mb**.
3. An average user time of **4.0 seconds** is needed to make a prediction.
4. An average elapsed time of **3.4 seconds** is needed to make a prediction.
5. The accuracy of the predictive model is approximate **17%** based on a test set.

Future Improvement
========================================================
1. Improve efficiency and accuracy of the predictive text model in order to provide a reasonable experience to the user.
2. Reduce the runtime of the predictive text model - the amount of time the algorithm takes to make a prediction given the acceptable input.
3. Reduce the size required by the predictive text model - the amount of memory (physical RAM) required to run the model.
