# yesodweb.com

![Runtime image](https://github.com/yesodweb/yesodweb.com/workflows/Runtime%20image/badge.svg)

You made it!  You're currently looking behind the scenes at
[yesodweb.com](http://yesodweb.com), the project page for Yesod.
I'm not going to explain what Yesod is, that you learn when you point
your browser to the link in the previous sentence.

The project page of Yesod is powered by... Yesod!  And in this repository
you can see how that is done: a good place to see Yesod applied in
practice.


## Installing

### Stack

Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/#how-to-intahttps://docs.haskellstack.org/en/stable/README/#how-to-install) and that should take care of installing GHC
and other dependencies.

### Getting yesodweb.com

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
    stack build

### Runnning

Easy too...

    stack exec yesodweb -- Development --port 3000

Now point your browser to `0.0.0.0:3000`

Enjoy!
