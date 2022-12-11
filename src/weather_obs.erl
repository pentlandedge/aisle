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
    Day:5,Hr:5,Min:6,Pres:4,Vis:1,_HVis:7,_RelHum:7,_AveWS:7,_WindDir:9,
    _AirP:9,_Rem/bitstring>>) ->
    {ok, #weather_obs_non_wmo{
        message_type = aisle:decode_message_type(MT),
        repeat_indicator = aisle:decode_repeat_indicator(RI),
        mmsi = MMSI,
        dac = DAC,
        fid = FID,
        variant = 0,
        location = decode_location(Loc),

        utc_day = decode_utc_day(Day),
        utc_hour = decode_utc_hour(Hr),
        utc_minute = decode_utc_minute(Min),
        present_weather = decode_wmo_code(Pres),
        visibility_limit = decode_vis_limit(Vis)
    }}.

decode_location(Bin) ->
    [sixbit:decode(X) || <<X:6>> <= Bin].

decode_utc_day(0) -> not_available;
decode_utc_day(X) when X > 0, X =< 31 -> X.

decode_utc_hour(24) -> not_available;
decode_utc_hour(X) when X >= 0, X < 24 -> X.

decode_utc_minute(60) -> not_available;
decode_utc_minute(X) when X >= 0, X < 60 -> X.

decode_wmo_code(0) -> clear;
decode_wmo_code(1) -> cloudy;
decode_wmo_code(2) -> rain;
decode_wmo_code(3) -> fog;
decode_wmo_code(4) -> snow;
decode_wmo_code(5) -> typhoon_hurricane;
decode_wmo_code(6) -> monsoon;
decode_wmo_code(7) -> thunderstorm;
decode_wmo_code(8) -> not_available;
decode_wmo_code(X) when X >= 9, X =< 15 -> reserved.

decode_vis_limit(0) -> limit_not_reached;
decode_vis_limit(1) -> limit_reached.
