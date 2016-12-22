# aisle
AIS Library in Erlang. Free software decoder library for the maritime AIS data format written in the Erlang programming language. This library is still in the early stages of development, but can now decode the main part of the AIS sentence and the message types containing the "Common Navigation Block" as it is termed in the protocol decoding notes. This is sufficient to pick up location and direction of travel information from vessels. It can now also decode base station reports and work on the aid to navigation report is underway.

## Protocol decoding
The information used to create this library is based heavily on the notes at http://catb.org/gpsd/AIVDM.html

## Test data
There is now some sample data in the logs directory. The first batch was captured with a receiver located at South Queensferry. 

## Building and runnning the unit tests
It is necessary to have Erlang installed, and the compiler erlc available on the path. The rebar tool is used to control the build process, so it is also necessary to have a copy of rebar available on the path. The software can be built (on a Linux platform) using rebar:
```
# rebar compile eunit
```

## Testing interactively
The easiest way to play around with the library is from the Erlang interactive shell. To start the shell and set up the paths to the compiled files, run
```
# rebar shell
```
From the Erlang shell, it is possible to parse the supplied log file, e.g.
```
1> A = aisle:parse_file("logs/ais_squeensferry_advansea_rx_100_20161120.log").
```
This will return a list of decoded records (the message types that are not understood are skipped). The contents of these records can then be displayed in an easier to read form:
```
2> aisle:display(A). 
```
Single sentences can be decoded and displayed in a similar manner:
```
3> B = aisle:decode("!AIVDM,1,1,,B,177KQJ5000G?tO`K>RA1wUbN0TKH,0*5C").
...
4> aisle:display(B).
...
```

