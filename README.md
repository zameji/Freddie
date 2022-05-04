## UPDATE 2022-05-05
I was able to make this run with a lot of pain under R 3.5.1
The steps to do this are sketched in Deploy.R; the environment (pretty much the deployment setup run on vanilla 3.5.1 is in environment.csv)
Superficial testing suggests that most of the stuff still works 🤯

Particular painpoint was installing old versions of packages. But there are apparently some breaking changes that mean the code won't run.

Without migrating to up-to-date packages, anything can happen. The codebase is not really supported anymore, TBH. But if anyone feels like cleaning it up and dealing with the tech-debt, I'd appreciate that deeply.

## Summary
The app is now deployed on [shinyapps.io](https://zameji.shinyapps.io/freddie-master/) This is mostly for sentimental value, so just on the free tier.

# Freddie Shiny


This is the code of a simple Shiny server used within university environment to help beginner students with carrying out statistical analyses.

It takes the data in a CSV file and visualises the variables in it as well as their relationship using the ggplot2 package.

Statistical tests are automatically chosen and performed with minimal input from the students.

The theme is based on Bootswatch.com
