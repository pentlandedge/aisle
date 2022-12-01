-module(ship_type).

-export([decode_ship_type/1]).

decode_ship_type(0) -> not_available;
decode_ship_type(X) when X >= 1, X =< 19 -> reserved;
decode_ship_type(X) when X >= 95, X =< 98 -> other_type_reserved;
decode_ship_type(99) -> other_no_info.

