#!/bin/sh

tar -cf emas.tar.gz ebin/
tar -cf skel.tar.gz deps/skel/ebin
ssh olek@titanic.di.unipi.it "rm -r ebin; rm -r deps"
scp emas.tar.gz olek@titanic.di.unipi.it:
scp skel.tar.gz olek@titanic.di.unipi.it:
ssh olek@titanic.di.unipi.it "tar -xf emas.tar.gz; tar -xf skel.tar.gz; chmod -R 755 ebin; chmod -R 755 deps; rm emas.tar.gz skel.tar.gz"
rm emas.tar.gz skel.tar.gz
