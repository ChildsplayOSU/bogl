##
## Release script for Windows
## Run this in GitBash, works just fine so long
## as 'stack' has already been installed.
## The entire process from scratch will take about 10-20 minutes
##

echo ""
echo -e "\033[92mBuilding 'spielserver' binary for Windows release\033[0m"
echo ""

## Only thing this needs in advance is stack to be installed

## fresh build with static compilation on
stack clean
stack build --ghc-options -static -optl-static

## install to local bin
stack install

## move binary to cur location
cp $HOME/AppData/Roaming/local/bin/spielserver.exe .

## no dep verification here...

echo ""
echo -e "\033[92mDone building Windows release\033[0m"
echo ""
