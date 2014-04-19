How can I determine the url that a local git repo was originally cloned from?
git config --get remote.origin.url
git remote show origin
git removte -v
git ls-remote --get-url

ssh-keygen -t rsa -C pabloxrios # no hace falta configurar passphrase para la RSA key
clip < ~/.ssh/id_rsa.pub
ssh -vT git@github.com

git remote rm origin  #Removes old origin
git remote add origin ssh://git@github.com/AutomatedTrading/r-strategies
git push --set-upstream origin master
git pull origin master
