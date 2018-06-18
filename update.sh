git fetch upstream
git reset --hard origin/master
git checkout master
git merge upstream/master

systemctl restart shiny-server