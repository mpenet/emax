sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
sudo apt install build-essential libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo \
                 librsvg2-dev \
                 libjansson4 libjansson-dev \
                 libgccjit0 libgccjit-11-dev gcc-11 g++-11
./autogen.sh
export CC=/usr/bin/gcc-11 CXX=/usr/bin/gcc-11
./configure --with-native-compilation --with-json --with-pgtk --with-librsvg
make -j8
sudo make install
