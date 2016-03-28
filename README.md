## macroCount Shiny app

**Version**: 0.7

**Notes**: A proof of concept of porting the [macroCount R script](https://github.com/biodim/MacroCount) to Shiny. Code is a bit of a mess, since I didn't do a complete rewrite. Does't have the "reactive" features of Shiny (yet). Check the screenshots folder to see how the macroCount Shiny app looks

Installation
------------

1) Install the Shiny Server (and configure it, following their Readme)

2) drop the macroCount folder into default app directory and launch the shiny server, usually done with:


```
sudo -u shiny shiny-server
```

3) To access the app: ip:3838/macroCount (unless you configured a different port)

BUT....

4) You will notice that the macroCount app will instantly crash. 

in chrome, type CTRL+SHIFT+J to open the javascript console and you will see that the crash is due to 
"plyr" not being properly installed. Your shiny user doesnt have admin privilages so it cant install libraries. You have to do it manually:

```
#Start R as admin
sudo R

#get Library Path
.libPaths()

#install the packages there
#ggplot2
install.packages("ggplot2", lib="/usr/lib/R/library")

#plyr
install.packages("ggplot2", lib="/usr/lib/R/library")
```

5) now if you go to ip:3838/macroCount, everything should be working fine

6) If you are hosting this in another country, you might want to change your R timezone:

```
R

#type (in R) to get directory
R.home()

#Exit R and go to console
cd /usr/lib/R/

#You want to find the 'Renviron.site' file
#For me it is in /usr/lib/R/etc/

cd /usr/lib/R/etc/

nano Renviron.site

#Add
TZ="timezone"

```

Save, verify in R and restart Shiny Server (just in case)

7) You might also want to use apache/nginx to password protect your app, raise an issue and Ill write a small tutorial to do ths
