#!/bin/bash

cabal install AbstractChannel/ --force-reinstall
#cabal install ~/protocolImplementation/protocolImplementation/shared/bytestringJSON/ --force-reinstall

cabal install Communication/ChannelInstances/VChan/ --force-reinstall
cabal install Communication/ChannelInstances/Http/ --force-reinstall
cabal install Communication/ChannelInstances/HttpTuna/ --force-reinstall
cabal install ArmoredCom/ --force-reinstall

#cabal build Communication/ChannelInstances/Http/httptesting/
