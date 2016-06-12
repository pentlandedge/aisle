%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2016 Robert Forbes.
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. 
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software 
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT 
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the 
%% License for the specific language governing permissions and limitations 
%% under the License.
%%

-module(aisle_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the aisle decoder. 
aisle_test_() ->
    [decode_sample1(), decode_bad_id()].

decode_sample1() ->
    Sentence = sample_sentence1(),
    Result = aisle:decode(Sentence),
    [?_assertEqual(ok, Result)].

decode_bad_id() ->
    Sentence = bad_identifier(),
    Result = aisle:decode(Sentence),
    [?_assertEqual({error, bad_identifier}, Result)].

sample_sentence1() -> "!AIVDM,1,1,,B,177KQJ5000G?tO`K>RA1wUbN0TKH,0*5C".

bad_identifier() -> "!AIDVM,1,1,,B,177KQJ5000G?tO`K>RA1wUbN0TKH,0*5C".

