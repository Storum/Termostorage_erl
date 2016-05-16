%% -*- coding: utf-8 -*-
-module(termo_gs_runner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("records.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).



%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export ([
            init/1, 
            handle_call/3, 
            handle_cast/2, 
            handle_info/2,
            terminate/2, 
            code_change/3
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link (Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


init ([Wialon_token]) ->

    Reprt_template_name = util:get_ev (termo, report_template_name, <<"Termostorage2">>),

                                                                                                        % получаем стартовые параметры работы
    {ok, #params_of_working {
                            sid                 = Sid, 
                            resource_id         = Resource_id, 
                            report_template_id  = Report_template_id, 
                            interval            = Tracing_interval_value, 
                            unit_list           = Unit_list}} = get_all_params (Wialon_token, Reprt_template_name, []),

                                                                                                        % запускаем первую обработку списка обхектов
    {ok, New_finish_time, New_unit_list} = check_all_units (Unit_list, Sid, Resource_id, Report_template_id, null, Tracing_interval_value),

    wialon_api:wh_logout (Sid),
    
    Timer = erlang:send_after (Tracing_interval_value, self(), interval),                                % запускаем таймер для отработки опроса wialon. Обработка таймера в handle_info(interval, State)


	{ok, #state {
                token                   = Wialon_token,
                resource_id             = Resource_id,
                report_template_id      = Report_template_id,
                unit_list               = New_unit_list,
                last_finish_time        = New_finish_time,
                interval                = Tracing_interval_value,
                report_template_name    = Reprt_template_name,
                timer                   = Timer}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.





%% Обрабатываем сигнал от таймера, по которому будем делать опрос сервера
%% ----------------------------------------------------------------------

handle_info(interval, #state {  
                                token                   = Wialon_token,
                                resource_id             = Resource_ID, 
                                report_template_id      = Report_id, 
                                unit_list               = Unit_list, 
                                last_finish_time        = Last_finish_time, 
                                interval                = Interval,
                                report_template_name    = Reprt_template_name,
                                timer                   = Timer} = State)->
    

    % {ok, Sid} = wialon_api:get_wh_sid (atom_to_list (Wialon_token)),      % получаем sid к wialon-у

    % {ok, New_unit_list} = wialon_api:get_wh_unit_list (Sid),
    % {ok, Reloaded_unit_list} = util:reload_list (New_unit_list, Unit_list),

    

    {ok, #params_of_working {
                            sid                 = Sid, 
                            resource_id         = Resource_ID, 
                            report_template_id  = Report_id, 
                            interval            = Tracing_interval_value, 
                            unit_list           = Reloaded_unit_list}} = get_all_params (Wialon_token, Reprt_template_name, Unit_list),


    io:format("~p ~n", [{tick, Tracing_interval_value, calendar:local_time()}]),

    {ok, New_finish_time, Unit_list_with_new_errors} = check_all_units (Reloaded_unit_list, Sid, Resource_ID, Report_id, Last_finish_time, Interval),
    

    wialon_api:wh_logout (Sid),


    erlang:cancel_timer (Timer),
    New_timer = erlang:send_after (Tracing_interval_value, self(), interval),

    {noreply, State#state {  
                            resource_id         = Resource_ID, 
                            report_template_id  = Report_id, 
                            unit_list           = Unit_list_with_new_errors, 
                            last_finish_time    = New_finish_time, 
                            interval            = Interval,
                            timer               = New_timer}};

handle_info(_Info, State) ->
    {noreply, State}.



% получаем все параметры с wialon-а
get_all_params (Token, Reprt_template_name, []) ->
    

    {ok, Sid} = wialon_api:get_wh_sid (atom_to_list (Token)),      % получаем sid к wialon-у
    

    {ok, {Resource_ID, Report_id}} = wialon_api:get_wh_find_report_data (Sid, Reprt_template_name),
    {ok, Tracing_interval_value} = wialon_api:get_report_interval_tracing (Sid, Resource_ID, Report_id, 300000),
    {ok, Unit_list} = wialon_api:get_wh_unit_list (Sid),

    {ok, #params_of_working {sid = Sid, resource_id = Resource_ID, report_template_id = Report_id, interval = Tracing_interval_value, unit_list = Unit_list}};


get_all_params (Token, Reprt_template_name, Old_unit_list) ->
    {ok, Sid} = wialon_api:get_wh_sid (atom_to_list (Token)),      % получаем sid к wialon-у
    {ok, {Resource_ID, Report_id}} = wialon_api:get_wh_find_report_data (Sid, Reprt_template_name),
    {ok, Tracing_interval_value} = wialon_api:get_report_interval_tracing (Sid, Resource_ID, Report_id, 300000),

    {ok, New_unit_list} = wialon_api:get_wh_unit_list (Sid),
    {ok, Reloaded_unit_list} = util:reload_list (New_unit_list, Old_unit_list),

    {ok, #params_of_working {sid = Sid, resource_id = Resource_ID, report_template_id = Report_id, interval = Tracing_interval_value, unit_list = Reloaded_unit_list}}.





check_all_units (Unit_list, Sid, Resource_ID, Report_id, null, Interval) ->
    Current_time = util:get_current_unixtime(),
    check_next_unit (Unit_list, Sid, Resource_ID, Report_id, Current_time, Interval, []);

check_all_units (Unit_list, Sid, Resource_ID, Report_id, Last_finish_time, Interval) ->
    check_next_unit (Unit_list, Sid, Resource_ID, Report_id, Last_finish_time, Interval, []).




check_next_unit ([#unit{} = UnitData | Tail], Sid, Resource_ID, Report_id, Last_finish_time, Interval, New_unit_list) ->

    Unit_id = UnitData#unit.unit_id,

    Trasing = wialon_api:exec_wh_report_batch (Sid, Resource_ID, Report_id, Unit_id, Last_finish_time - (Interval div 1000) - 10, Last_finish_time),

    {ok, List_of_trasing_data} = wialon_api:get_report_termo_2_tracing_list (Trasing, []),
    

    {ok, Unit_data_humid} = check_trasing_data_humid (UnitData, List_of_trasing_data, (UnitData#unit.last_humid_error)#last_humid_error.error),
    {ok, Unit_data_termo} = check_trasing_data_termo (UnitData, List_of_trasing_data, (UnitData#unit.last_termo_error)#last_termo_error.error),



    New_unit_data = #unit {
                            unit_id = UnitData#unit.unit_id,
                            unit_name = UnitData#unit.unit_name,
                            unit_params = UnitData#unit.unit_params,
                            last_humid_error = Unit_data_humid#unit.last_humid_error,
                            last_termo_error = Unit_data_termo#unit.last_termo_error
                            },

    Concat_new_unit_list = lists:merge (New_unit_list, [New_unit_data]),


    check_next_unit (Tail, Sid, Resource_ID, Report_id, Last_finish_time, Interval, Concat_new_unit_list);

check_next_unit ([], _Sid, _Resource_ID, _Report_id, Last_finish_time, Interval, Concat_new_unit_list) -> 
    {ok, Last_finish_time + (Interval div 1000), Concat_new_unit_list}.





check_trasing_data_humid (#unit{} = UnitData, [#tracing{} = Tracing | Tail], LastError)  when Tracing#tracing.humid < (UnitData#unit.unit_params)#unit_params.humid_min ->
    check_trasing_data_humid (UnitData#unit {last_humid_error = #last_humid_error {error=min_limit_disrupted, humid = Tracing#tracing.humid, date = Tracing#tracing.date}}, Tail, LastError);

check_trasing_data_humid (#unit{} = UnitData, [#tracing{} = Tracing | Tail], LastError)  when Tracing#tracing.humid > (UnitData#unit.unit_params)#unit_params.humid_max ->
    check_trasing_data_humid (UnitData#unit {last_humid_error = #last_humid_error {error = max_limit_disrupted, humid = Tracing#tracing.humid, date = Tracing#tracing.date}}, Tail, LastError);

check_trasing_data_humid (#unit{} = UnitData, [#tracing{} = Tracing | Tail], LastError)  when Tracing#tracing.humid > (UnitData#unit.unit_params)#unit_params.humid_min; Tracing#tracing.humid < (UnitData#unit.unit_params)#unit_params.humid_max ->
    check_trasing_data_humid (UnitData#unit {last_humid_error = #last_humid_error {error = no_error, humid = Tracing#tracing.humid, date = Tracing#tracing.date}}, Tail, LastError);

check_trasing_data_humid (#unit{} = UnitData, [], LastError) when (UnitData#unit.last_humid_error)#last_humid_error.error =/= LastError ->
    {ok, Msg} = create_sms_message_humid (UnitData#unit.last_humid_error, UnitData#unit.unit_name, (UnitData#unit.unit_params)#unit_params.humid_min, (UnitData#unit.unit_params)#unit_params.humid_max),    
    util:send_mail2sms ((UnitData#unit.unit_params)#unit_params.phone_list, Msg),
    io:format("~ts ~n", [Msg]),
    {ok, UnitData};

check_trasing_data_humid (#unit{} = UnitData, [], LastError) when (UnitData#unit.last_humid_error)#last_humid_error.error =:= LastError ->  
    {ok, UnitData}.



check_trasing_data_termo (#unit{} = UnitData, [#tracing{} = Tracing | Tail], LastError)  when Tracing#tracing.termo < (UnitData#unit.unit_params)#unit_params.termo_min ->
    check_trasing_data_termo (UnitData#unit {last_termo_error = #last_termo_error {error = min_limit_disrupted, termo = Tracing#tracing.termo, date = Tracing#tracing.date}}, Tail, LastError);

check_trasing_data_termo (#unit{} = UnitData, [#tracing{} = Tracing | Tail], LastError)  when Tracing#tracing.termo > (UnitData#unit.unit_params)#unit_params.termo_max ->
    check_trasing_data_termo (UnitData#unit {last_termo_error = #last_termo_error {error = max_limit_disrupted, termo = Tracing#tracing.termo, date = Tracing#tracing.date}}, Tail, LastError);

check_trasing_data_termo (#unit{} = UnitData, [#tracing{} = Tracing | Tail], LastError)  when Tracing#tracing.termo > (UnitData#unit.unit_params)#unit_params.termo_min; Tracing#tracing.termo < (UnitData#unit.unit_params)#unit_params.termo_max ->
    check_trasing_data_termo (UnitData#unit {last_termo_error = #last_termo_error {error = no_error, termo = Tracing#tracing.termo, date = Tracing#tracing.date}}, Tail, LastError);

check_trasing_data_termo (#unit{} = UnitData, [], LastError) when (UnitData#unit.last_termo_error)#last_termo_error.error =/= LastError ->
    {ok, Msg} = create_sms_message_termo (UnitData#unit.last_termo_error, UnitData#unit.unit_name, (UnitData#unit.unit_params)#unit_params.termo_min, (UnitData#unit.unit_params)#unit_params.termo_max),
    util:send_mail2sms ((UnitData#unit.unit_params)#unit_params.phone_list, Msg),
    io:format("~ts ~n", [Msg]),
    {ok, UnitData};

check_trasing_data_termo (#unit{} = UnitData, [], LastError) when (UnitData#unit.last_termo_error)#last_termo_error.error =:= LastError ->  
    {ok, UnitData}.




create_sms_message_humid (#last_humid_error {error = Error, humid = Humid, date = Date}, Unit_name, HumidMin, HumidMax) when Error =:= no_error->
    {ok, Unit_name  ++ " НОРМА. Влажность " ++ mochinum:digits(Humid) ++ "% в " ++ util:convert_wh_date (Date) ++ ". Границы нормы: от " ++ mochinum:digits(HumidMin) ++ "% до " ++ mochinum:digits(HumidMax) ++  "%"};

create_sms_message_humid (#last_humid_error {error = Error, humid = Humid, date = Date}, Unit_name, HumidMin, HumidMax) when Error =:= min_limit_disrupted ->
    {ok, Unit_name  ++ " НАРУШЕНИЕ. Влажность " ++ mochinum:digits(Humid) ++ "% в " ++ util:convert_wh_date (Date) ++ ". Границы нормы: от " ++ mochinum:digits(HumidMin) ++ "% до " ++ mochinum:digits(HumidMax) ++  "%"};

    %{ok, "На складе [" ++ Unit_name  ++ "] превышен минимальный уровень влажности (Минимальный уровень влажности: " ++ mochinum:digits(HumidMin) ++ "); Значение влажности: " ++ mochinum:digits(Humid) ++ "; [Время: " ++ util:convert_wh_date (Date) ++ "]"};

create_sms_message_humid (#last_humid_error {error = Error, humid = Humid, date = Date}, Unit_name, HumidMin, HumidMax) when Error =:= max_limit_disrupted ->
    {ok, Unit_name  ++ " НАРУШЕНИЕ. Влажность " ++ mochinum:digits(Humid) ++ "% в " ++ util:convert_wh_date (Date) ++ ". Границы нормы: от " ++ mochinum:digits(HumidMin) ++ "% до " ++ mochinum:digits(HumidMax) ++  "%"}.
    %{ok, "На складе [" ++ Unit_name  ++ "] превышен максмальный уровень влажности (Максимальный уровень влажности: " ++ mochinum:digits(HumidMax) ++ "); Значение влажности: " ++ mochinum:digits(Humid) ++ "; [Время: " ++ util:convert_wh_date (Date) ++ "]"}.



create_sms_message_termo (#last_termo_error {error = Error, termo = Termo, date = Date}, Unit_name, TermoMin, TermoMax) when Error =:= no_error ->
    {ok, Unit_name  ++ " НОРМА. Температура " ++ mochinum:digits(Termo) ++ "°С в " ++ util:convert_wh_date (Date) ++ ". Границы нормы: от " ++ mochinum:digits(TermoMin) ++ "°С до " ++ mochinum:digits(TermoMax) ++  "°С"};
    %{ok, "На складе [" ++ Unit_name  ++ "] нормализован уровень температуры (Минимальный уровень температуры: " ++ mochinum:digits(TermoMin) ++ "; Максимальный уровень температуры: " ++ mochinum:digits(TermoMax) ++ "); Текущий уровень температуры: " ++ mochinum:digits (Termo) ++ " [Время: " ++ util:convert_wh_date (Date) ++ "]"};

create_sms_message_termo (#last_termo_error {error = Error, termo = Termo, date = Date}, Unit_name, TermoMin, TermoMax) when Error =:= min_limit_disrupted ->
    {ok, Unit_name  ++ " НАРУШЕНИЕ. Температура " ++ mochinum:digits(Termo) ++ "°С в " ++ util:convert_wh_date (Date) ++ ". Границы нормы: от " ++ mochinum:digits(TermoMin) ++ "°С до " ++ mochinum:digits(TermoMax) ++  "°С"};
    %{ok, "На складе [" ++ Unit_name  ++ "] превышен минимальный уровень температуры (Минимальный уровень температуры: " ++ mochinum:digits(TermoMin) ++ "); Значение температуры: " ++ mochinum:digits(Termo) ++ "; [Время: " ++ util:convert_wh_date (Date) ++ "]"};

create_sms_message_termo (#last_termo_error {error = Error, termo = Termo, date = Date}, Unit_name, TermoMin, TermoMax) when Error =:= max_limit_disrupted ->
    {ok, Unit_name ++ " НАРУШЕНИЕ. Температура " ++ mochinum:digits(Termo) ++ "°С в " ++ util:convert_wh_date (Date) ++ ". Границы нормы: от " ++ mochinum:digits(TermoMin) ++ "°С до " ++ mochinum:digits(TermoMax) ++  "°С"}.
    %{ok, "На складе [" ++ Unit_name  ++ "] превышен максмальный уровень температуры (Максимальный уровень температуры: " ++ mochinum:digits(TermoMax) ++ "); Значение температуры: " ++ mochinum:digits(Termo) ++ "; [Время: " ++ util:convert_wh_date (Date) ++ "]"}.







terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

