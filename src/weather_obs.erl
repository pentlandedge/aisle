-module(weather_obs).

-export([decode/1]).

-record(weath_obs, {
    message_type,
    repeat_indicator,
    mmsi
    }).

decode(_Bin) ->
    {ok, #weath_obs{}}.
