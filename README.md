# Concurrent railroad simulator in Ada #

### Compilation: ###
`gnatmake main.adb`
### Usage: ###
`./main [OPTIONS]`

where options are:
```
input filename
    read configuration data from file specified by `filename` (default `input`)
verbose true/false
    turn verbose mode on or off (off by default)
```

#### Configuration file: ####
Configuration file defines railroad characteristics such as:
* simulation start clock,
* how many seconds should hour simulation take,
* specification of all tracks: turntables, station and normal tracks,
* specification of trains together with their route.

Example configuration file can be found in `input` with further instructions on how to write such file.
