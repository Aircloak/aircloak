### JavaScript Sandbox

This folder contains the sandbox for executing JavaScript code over results published through Airpub.
The sandbox is implemented as a wrapper over the SpiderMonkey engine and exposed to Erlang as an external port.

Because of the size of the SpiderMonkey code base and in order to accelerate build times, we only include here
the required headers and the pre-compiled engine as dynamic library.

The following steps were done in order to obtain the included files (replace <repository_path>
with the actual path of the current repository):
```
mkdir temp
cd temp
wget https://people.mozilla.org/~sstangl/mozjs-38.2.1.rc0.tar.bz2
bunzip2 mozjs-38.2.1.rc0.tar.bz2
tar -xf mozjs-38.2.1.rc0.tar
cd mozjs-38.0.0/js/src
./configure
make
strip dist/lib/libmozjs-.so
cp dist/lib/libmozjs-.so <repository_path>/backend/apps/airpub/js_sandbox/js_engine/bin/
cp -R dist/include <repository_path>/backend/apps/airpub/js_sandbox/js_engine/
```
The `temp` folder can be safely deleted afterwards.


**Building on MacOS**

You need to re-build the SpiderMonkey engine library for MacOS by executing the steps above,
just replace `.so` with `.dylib`. You don't need to copy the `include` folder also.
