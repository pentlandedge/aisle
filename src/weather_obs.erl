-module(weather_obs).

-export([decode/1]).

-record(weather_obs, {
    message_type,
    repeat_indicator,
    mmsi,
    dac,
    fid,
    variant,
    location,
    longitude,
    latitude
    }).

-opaque weather_obs() :: #weather_obs{}.
-export_type([weather_obs/0]).

%% @doc Decode the IMO289 weather observations. 
-spec decode(binary()) -> {ok, weather_obs()} | {error, Reason::atom()}.
decode(<<MT:6,RI:2,MMSI:30,DAC:10,FID:6,V:1,_Rem/bitstring>>) ->
    {ok, #weather_obs{
        message_type = aisle:decode_message_type(MT),
        repeat_indicator = aisle:decode_repeat_indicator(RI),
        mmsi = MMSI,
        dac = DAC,
        fid = FID,
        variant = V 
    }}.
