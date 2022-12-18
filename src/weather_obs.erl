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

-record(weather_obs_wmo, {
    message_type,
    repeat_indicator,
    mmsi,
    dac,
    fid,
    variant,
    location,
    longitude,
    latitude,
    utc_month,
    utc_day,
    utc_hour,
    utc_minute,
    course_over_ground,
    speed_over_ground,
    heading,
    pressure_sea_level,
    pressure_change,
    pressure_tendency,
    true_wind_direction,
    true_wind_speed,
    relative_wind_direction,
    relative_wind_speed,
    maximum_gust_speed,
    maximum_gust_direction,
    air_temperature,
    relative_humidity,
    sea_surface_temperature,
    horiz_visibility,
    present_weather,
    past_weather_1,
    past_weather_2,
    total_cloud_cover,
    cloud_amount_low,
    cloud_type_low,
    cloud_type_middle,
    cloud_type_high,
    height_of_cloud_base,
    period_of_wind_waves,
    height_of_wind_waves,
    first_swell_direction,
    first_swell_period,
    first_swell_height,
    second_swell_direction,
    second_swell_period,
    second_swell_height,
    ice_deposit_thickness,
    rate_of_ice_accretion,
    cause_of_ice_accretion,
    sea_ice_concentration,
    amount_type_ice,
    ice_situation,
    ice_development,
    bearing_of_ice_edge}).





-opaque weather_obs_non_wmo() :: #weather_obs_non_wmo{}.
-export_type([weather_obs_non_wmo/0]).

%% @doc Decode the IMO289 weather observations. 
-spec decode(binary()) -> {ok, weather_obs_non_wmo()} | {error, Reason::atom()}.
decode(<<MT:6,RI:2,MMSI:30,DAC:10,FID:6,0:1,Loc:120/bitstring,Lon:25/signed,
    Lat:24/signed,Day:5,Hr:5,Min:6,Pres:4,Vis:1,HVis:7,RelHum:7,AveWS:7,
    WindDir:9,AirP:9,Tend:4,AirT:11,WatT:10,WavP:6,WavH:8,WavD:9,SwH:8,SwD:9,
    SwP:6,_Sp:3,_Rem/bitstring>>) ->
    {ok, #weather_obs_non_wmo{
        message_type = aisle:decode_message_type(MT),
        repeat_indicator = aisle:decode_repeat_indicator(RI),
        mmsi = MMSI,
        dac = DAC,
        fid = FID,
        variant = 0,
        location = decode_location(Loc),
        longitude = decode_longitude(Lon),
        latitude = decode_latitude(Lat),
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
        pressure_tendency = Tend, % Need to find how to interpret this.
        air_temperature = decode_air_temperature(AirT),
        water_temperature = decode_water_temperature(WatT),
        wave_period = decode_wave_period(WavP),
        wave_height = decode_wave_height(WavH),
        wave_direction = decode_wave_direction(WavD),
        swell_height = decode_swell_height(SwH),
        swell_direction = decode_swell_direction(SwD),
        swell_period = decode_swell_period(SwP)}};
decode(<<MT:6,RI:2,MMSI:30,DAC:10,FID:6,1:1,Lon:16,Lat:16,Mon:4,Day:6,Hr:5,
    Min:3,COG:7,SOG:5,Hd:7,PSL:11,PC:20,PT:4,WD:7,WS:8,RWD:7,RWS:8,
    _Rem/bitstring>>) ->
    {ok, #weather_obs_wmo{
        message_type = aisle:decode_message_type(MT),
        repeat_indicator = aisle:decode_repeat_indicator(RI),
        mmsi = MMSI,
        dac = DAC,
        fid = FID,
        variant = 1,
        longitude = decode_wmo_longitude(Lon),
        latitude = decode_wmo_latitude(Lat),
        utc_month = decode_wmo_utc_month(Mon),
        utc_day = decode_wmo_utc_day(Day),
        utc_hour = decode_wmo_utc_hour(Hr),
        utc_minute = decode_wmo_utc_minute(Min),
        course_over_ground = decode_course_over_ground(COG),
        speed_over_ground = decode_speed_over_ground(SOG),
        heading = decode_heading(Hd),
        pressure_sea_level = decode_pressure_sea_level(PSL),
        pressure_change = decode_pressure_change(PC),
        pressure_tendency = decode_pressure_tendency(PT),
        true_wind_direction = decode_true_wind_direction(WD),
        true_wind_speed = decode_true_wind_speed(WS),
        relative_wind_direction = decode_relative_wind_direction(RWD),
        relative_wind_speed = decode_relative_wind_speed(RWS)
        % maximum_gust_speed,
        % maximum_gust_direction,
        % air_temperature,
        % relative_humidity,
        % sea_surface_temperature,
        % horiz_visibility,
        % present_weather,
        % past_weather_1,
        % past_weather_2,
        % total_cloud_cover,
        % cloud_amount_low,
        % cloud_type_low,
        % cloud_type_middle,
        % cloud_type_high,
        % height_of_cloud_base,
        % period_of_wind_waves,
        % height_of_wind_waves,
        % first_swell_direction,
        % first_swell_period,
        % first_swell_height,
        % second_swell_direction,
        % second_swell_period,
        % second_swell_height,
        % ice_deposit_thickness,
        % rate_of_ice_accretion,
        % cause_of_ice_accretion,
        % sea_ice_concentration,
        % amount_type_ice,
        % ice_situation,
        % ice_development,
        % bearing_of_ice_edge,
    }};
decode(_) ->
    {error, failed_to_decode_svd}.
decode_location(Bin) ->
    [sixbit:decode(X) || <<X:6>> <= Bin].

decode_longitude(181000) -> not_available;
decode_longitude(X)      -> 0.001 * X.

decode_latitude(91000) -> not_available;
decode_latitude(X)     -> 0.001 * X.

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

decode_wind_direction(X) -> decode_direction(X).

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

decode_wave_period(X) -> decode_period(X).

decode_wave_height(X)  -> decode_height(X).

decode_wave_direction(X) -> decode_direction(X).

decode_swell_height(X) -> decode_height(X).

decode_swell_direction(X) -> decode_direction(X).

decode_swell_period(X) -> decode_period(X).

decode_direction(360) -> 
    not_available;
decode_direction(X) when X >= 0, X =< 359 -> 
    X.

decode_height(255) -> not_available;
decode_height(X)   -> 0.1 * X.

decode_period(63) -> not_available;
decode_period(X)  -> X.

decode_wmo_longitude(16#FFFF) -> not_available;
decode_wmo_longitude(X)       -> 0.01 * X - 180.0.

decode_wmo_latitude(16#7FFF) -> not_available;
decode_wmo_latitude(X)       -> 0.01 * X - 90.0.

decode_wmo_utc_month(15) ->
    not_available;
decode_wmo_utc_month(X) when X >= 1, X =< 12 ->
    X.

decode_wmo_utc_day(63) ->
    not_available;
decode_wmo_utc_day(X) when X >= 1, X =< 31 ->
    X.

decode_wmo_utc_hour(31) ->
    not_available;
decode_wmo_utc_hour(X) when X >= 0, X =< 23 ->
    X.

decode_wmo_utc_minute(7) ->
    not_available;
decode_wmo_utc_minute(X) when X >= 0, X =< 6 ->
    10 * X.

decode_course_over_ground(X) when X >= 0, X =< 359 -> X.

decode_speed_over_ground(31) ->
    not_available;
decode_speed_over_ground(X) when X >= 0, X =< 30 ->
    X.

%% @doc Decode the ship heading. Allows a value of zero which is not mentioned
%% in the notes (range 1 -> 72 pre-scaling permitted).
decode_heading(X) -> decode_bearing(X).

decode_pressure_sea_level(X) when X >= 0, X =< 2000 ->
    (X / 10) + 900.

%% @doc Decode pressure change function. Note the use of 1000 as the upper
%% limit, rather than 100 in the spec (which looks wrong, given the desired
%% output range).
decode_pressure_change(1023) ->
    not_available;
decode_pressure_change(X) when X >= 0, X =< 1000 ->
    (X / 10) - 50.

decode_pressure_tendency(0) -> increasing_then_decreasing;
decode_pressure_tendency(1) -> increasing_then_steady;
decode_pressure_tendency(2) -> increasing;
decode_pressure_tendency(3) -> decreasing_or_steady_then_increasing;
decode_pressure_tendency(4) -> steady;
decode_pressure_tendency(5) -> decreasing_then_increasing;
decode_pressure_tendency(6) -> decreasing_then_steady;
decode_pressure_tendency(7) -> decreasing;
decode_pressure_tendency(8) -> steady_or_increasing_then_decreasing;
decode_pressure_tendency(15) -> not_available.

decode_true_wind_direction(X) -> decode_bearing(X).

decode_true_wind_speed(X) -> decode_speed(X).

decode_relative_wind_direction(X) -> decode_bearing(X).

decode_relative_wind_speed(X) -> decode_speed(X).

%% @doc Decode a bearing. Allows a value of zero which is not mentioned
%% in the notes (range 1 -> 72 pre-scaling permitted).
decode_bearing(127) ->
    not_available;
decode_bearing(X) when X >= 0, X =< 72 ->
    5 * X.

decode_speed(255) ->
    not_available;
decode_speed(X) when X >= 0, X =< 254 ->
    0.5 * X.
