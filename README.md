# aisle
AIS Library in Erlang. Free software decoder library for the maritime AIS data format written in the Erlang programming language..

## Protocol decoding
The information used to create this library is based heavily on the notes at http://catb.org/gpsd/AIVDM.html

## Test data
There is now some sample data in the logs directory. The first batch was captured with a receiver located at South Queensferry. 

## Building and runnning the unit tests
It is necessary to have Erlang installed, and the compiler erlc available on the path. The rebar tool is used to control the build process, so it is also necessary to have a copy of rebar available on the path. The software can be built (on a Linux platform) using rebar:
```
# rebar compile eunit
```
