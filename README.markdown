# yesodweb.com

You made it!  You're currently looking behind the scenes at
[yesodweb.com](http://yesodweb.com), the project page for Yesod.
I'm not going to explain what Yesod is, that you learn when you point
your browser to the link in the previous sentence.

The project page of Yesod is powered by... Yesod!  And in this repository
you can see how that is done: a good place to see Yesod applied in
practice.


## Installing

### A recent GHC and HP

It is not hard science, but it seems that `yesodweb.com` needs GHC 7.0.4
or newer and [Haskell Platform](http://hackage.haskell.org/platform)
(HP) 2011.4 or newer.

For good measures we first remove all traces of a previous Haskell installation
(on Ubuntu `sudo apt-get remove ghc6` and `rm -rf ~/.cabal ~/.ghc` might
just be enough).

Then install GHC 7.0.4 or later.  If you wish to install from source
then you know probably what to do, installing a binary is [explained for
various operating systems](http://www.haskell.org/ghc/download_ghc_7_0_4#distros).

On Ubuntu is was as simple as:

    cd ~
    wget http://www.haskell.org/ghc/dist/7.0.4/ghc-7.0.4-x86_64-unknown-linux.tar.bz2
    # unpack and cd to dir
    ./configure
    sudo make install

Then install the HP.  On Ubuntu 11.04 we had to install from source:

    wget http://lambda.haskell.org/platform/download/2011.4.0.0/haskell-platform-2011.4.0.0.tar.gz
    # unpack and cd to dir
    ./configure
    make
    sudo make install

Now make sure that `~/.cabal/bin` is in the path.  On Ubuntu we do this by:

    echo $PATH|grep ".cabal/bin"  # should give some output

Finally we need to upgrade `alex` as HP ships a version that is too old.

    cabal install alex


### Getting yeshodweb.com

Since `yesodweb.com` changes often we advise anyone to get it through
`git` (available on [most platforms](http://git-scm.com/download)) so it
is easy to keep up to date.

First time install is as follows:

    git clone https://github.com/yesodweb/yesodweb.com.git
    cd yesodweb.com
    git submodule update --init

To update the repos do:

    cd yesodweb.com
    git pull && git submodule update


### Building

Easy...

    cd yesodweb.com
    cabal install


### Runnning

Easy too...

    yesodweb Development --port 3000

Now point your browser to `0.0.0.0:3000`

Enjoy!



