# copy config files
mkdir ~/.xmonad
cp ./xmonad.hs ~/.xmonad/
cp ./conkyrc ~/.xmonad/
cp -r ./lib/ ~/.xmonad/

# copy bin files
mkdir ~/bin
cp -r ./bin/* ~/bin
chmod +x ~/bin/*
echo "make sure ~/bin is in \$PATH"

# icons
mkdir -p ~/.xmonad/images/icons
tar -zxvf ./images.tar.gz
mv ./images/icons/* ~/.xmonad/images/icons/
rmdir ./images/icons
rmdir ./images
tar -zxvf ./thayer-icons.tar.gz
mv ./thayer/* ~/.xmonad/images/icons/
rmdir ./thayer
tar -zxvf ./xbm8x8-icons.tar.gz
mv ./xbm8x8/* ~/.xmonad/images/icons/
rmdir ./xbm8x8

echo "done.\nreload your xmonad config"
xmonad --recompile
