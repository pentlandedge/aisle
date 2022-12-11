-module(weather_obs).

-export([decode/1]).

-record(weather_obs_non_wmo, {
    message_type,
    repeat_indicator,
    mmsi,
    dac,
    fid,
    variant,
    location,
    longitude,
    latitude,
    utc_day,
    utc_hour,
    utc_minute,
    present_weather,
    visibility_limit,
    horiz_visibility,
    relative_humidity,
    average_wind_speed,
    wind_direction,
    air_pressure,
    pressure_tendency,
    air_temperature,
    water_temperature,
    wave_period,
    wave_height,
    wave_direction,
    swell_height,
    swell_direction,
    swell_period}).

-opaque weather_obs_non_wmo() :: #weather_obs_non_wmo{}.
-export_type([weather_obs_non_wmo/0]).

%% @doc Decode the IMO289 weather observations. 
-spec decode(binary()) -> {ok, weather_obs_non_wmo()} | {error, Reason::atom()}.
decode(<<MT:6,RI:2,MMSI:30,DAC:10,FID:6,0:1,Loc:120/bitstring,_Lon:25,_Lat:24,
    _Day:5,_Hr:5,Min:6,_Pres:4,_Vis:1,_HVis:7,_RelHum:7,_AveWS:7,_WindDir:9,
    _AirP:9,_Rem/bitstring>>) ->
    {ok, #weather_obs_non_wmo{
        message_type = aisle:decode_message_type(MT),
        repeat_indicator = aisle:decode_repeat_indicator(RI),
        mmsi = MMSI,
        dac = DAC,
        fid = FID,
        variant = 0,
        location = decode_location(Loc),

        utc_minute = decode_utc_minute(Min)
    }}.

decode_location(Bin) ->
    [sixbit:decode(X) || <<X:6>> <= Bin].

decode_utc_minute(60) -> not_available;
decode_utc_minute(X) when X >= 0, X =< 59 -> X.
