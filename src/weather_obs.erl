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
    Day:5,Hr:5,Min:6,Pres:4,Vis:1,HVis:7,RelHum:7,AveWS:7,WindDir:9,
    AirP:9,_Tend:4,AirT:11,WatT:10,_Rem/bitstring>>) ->
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
        visibility_limit = decode_vis_limit(Vis),
        horiz_visibility = decode_horiz_vis(HVis),
        relative_humidity = decode_relative_humidity(RelHum),
        average_wind_speed = decode_wind_speed(AveWS),
        wind_direction = decode_wind_direction(WindDir),
        air_pressure = decode_air_pressure(AirP),
        air_temperature = decode_air_temperature(AirT),
        water_temperature = decode_water_temperature(WatT)
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

decode_horiz_vis(127) -> 
    not_available;
decode_horiz_vis(X) when X >= 0, X =< 126 ->
    0.1 * X.

%% @doc Relative humidity as a percentage.
decode_relative_humidity(127) ->
    not_available;
decode_relative_humidity(X) when X >= 0, X =< 100 ->
    X.

decode_wind_speed(127) -> 
    not_available;
decode_wind_speed(X) when X >= 0 -> 
    X.

decode_wind_direction(360) -> 
    not_available;
decode_wind_direction(X) when X >= 0, X =< 359 -> 
    X.

%% @doc Decode the air pressure. The reference material suggests adding 400
%% to the value, but this seems wrong given the range of available input
%% numbers and the desired output range. 800 seems to fit the range, but
%% need to check this.
decode_air_pressure(402) -> pressure_gte_1201;
decode_air_pressure(403) -> not_available;
decode_air_pressure(X)   -> X + 800.

decode_air_temperature(1024) -> 
    not_available;
decode_air_temperature(X) when X >=0 -> 
    -60.0 + 0.1 * X.

decode_water_temperature(601) ->
    not_available;
decode_water_temperature(X) when X >=0, X =< 600 ->
    -10.0 + 0.1 * X.
