%% -*- coding: utf-8 -*-
-module(termo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	
	%io:format("~ts~n", [M]),


	application:start(inets),
	application:start(crypto), 
	application:start(asn1),
	application:start(public_key),
	application:start(ssl),
	application:start(gen_smtp),
    termo_sup:start_link().

	

stop(_State) ->
    ok.
