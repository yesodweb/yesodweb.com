#!/bin/bash -ex
#cabal clean
#cabal configure
#cabal build
strip dist/build/yesodweb/yesodweb
bzip2 dist/build/yesodweb/yesodweb
scp -r dist/build/yesodweb/yesodweb.bz2 ubuntu@www.yesodweb.com:/home/ubuntu/yesodweb.com
ssh ubuntu@www.yesodweb.com 'cd yesodweb.com mv yesodweb yesodweb.old && bunzip2 yesodweb.bz2 && sudo restart yesodweb_com'
