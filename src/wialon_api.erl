%% -*- coding: utf-8 -*-
-module(wialon_api).
-author('storumproject@gmail.com').
-include("records.hrl").



-export([   get_wh_sid/1, 
            get_wialon_response/1, 
            get_wh_unit_list/1, 
            get_wh_find_report_data/2, 
            exec_wh_report/6, 
            exec_wh_report_batch/6, 
            register_fuell/10, 
            get_report_rows/2,
            get_report_termo_2_tracing_list/2, 
            get_report_interval_tracing/4, 
            wh_logout/1
        ]).


get_wialon_response ({ok, {{_HTTP_Version, 200, _HTTP_status_phrase}, _HTTP_header, Response_from_wialon}}) ->
    get_response_data_list (element (2, mochijson2:decode (Response_from_wialon)), Response_from_wialon);

get_wialon_response ({ok, {{_HTTP_Version, _Other_status_code, HTTP_status_phrase}, _, _}}) ->
    {fail, {http_error, HTTP_status_phrase}};

get_wialon_response ({error, Error}) ->
    {fail, Error}.



get_response_data_list (Response_data_list, _Response_from_wialon) when is_list (Response_data_list) -> 
    {ok, Response_data_list};

get_response_data_list (_, Response_from_wialon) ->
    {fail, {other_error, Response_from_wialon}}.



wh_logout (Sid) ->
    httpc:request(post, 
                    {"http://hst-api.wialon.com/wialon/ajax.html?svc=core/logout", 
                        [], 
                        "application/x-www-form-urlencoded", 
                        "sid=" ++ Sid ++ "&params={}"}, 
                    [],
                    []),
    ok.



get_wh_sid (Token) ->
    
    Response = httpc:request(post, 
                    {"http://hst-api.wialon.com/wialon/ajax.html?svc=token/login", 
                        [], 
                        "application/x-www-form-urlencoded", 
                        "params={token:" ++ Token ++ "}"}, 
                    [],
                    []),

    case Response of
        {error, socket_closed_remotely} ->
            get_wh_sid (Token);
        _ ->
            BodyResponse = get_wialon_response (Response),
            get_sid_from_wh_response (BodyResponse, Token)
    end.
    

get_sid_from_wh_response ({ok, [_ | [{<<"eid">>, Sid} | _Tail]]}, _Token) ->
    {ok, binary_to_list (Sid)};

get_sid_from_wh_response ({ok, [{<<"error">>, _WialonErrorCode} | _]}, Token) ->
    get_wh_sid (Token).
    %{fail, {wialon_error, WialonErrorCode}}.








get_wh_unit_list (Sid) ->
    
    Response = httpc:request(post, 
                    {"http://hst-api.wialon.com/wialon/ajax.html?svc=core/search_items", 
                        [], 
                        "application/x-www-form-urlencoded", 
                        "sid=" ++ Sid ++ "&params={spec:{itemsType:avl_unit,propName:*,propValueMask:*,sortType:\"\",propType:property},force:1,flags:9,from:0,to:4294967295}"}, 
                    [],
                    []),

    BodyResponse = get_wialon_response (Response),
    
    get_unit_list_from_wh_response (BodyResponse).


get_unit_list_from_wh_response ({ok, [{<<"searchSpec">>,_} ,_ ,_ ,_ ,_ , {<<"items">>, Item_list}]}) ->
        
    Unit_list = lists:map ( fun ({struct, Fields}) -> 
                                Unit_id = proplists:get_value(<<"id">>, Fields),
                                Unit_name = unicode:characters_to_list (proplists:get_value(<<"nm">>, Fields)),
                                {struct, Addition_Field_list} = proplists:get_value(<<"flds">>, Fields),

                                
                                Addition_filed_element_HumidMax = lists:filter (   fun ({_, {struct, Add_fields_property_list}})   ->
                                                                                                                    N = proplists:get_value(<<"n">>, Add_fields_property_list),
                                                                                                                    N =:=  <<"HumidMax">>
                                                                                                            end, Addition_Field_list),

                                Addition_filed_element_HumidMin = lists:filter (   fun ({_, {struct, Add_fields_property_list}})   ->
                                                                                                                    N = proplists:get_value(<<"n">>, Add_fields_property_list),
                                                                                                                    N =:=  <<"HumidMin">>
                                                                                                            end, Addition_Field_list),

                                Addition_filed_element_TermoMax = lists:filter (   fun ({_, {struct, Add_fields_property_list}})   ->
                                                                                                                    N = proplists:get_value(<<"n">>, Add_fields_property_list),
                                                                                                                    N =:=  <<"TermoMax">>
                                                                                                            end, Addition_Field_list),

                                Addition_filed_element_TermoMin = lists:filter (   fun ({_, {struct, Add_fields_property_list}})   ->
                                                                                                                    N = proplists:get_value(<<"n">>, Add_fields_property_list),
                                                                                                                    N =:=  <<"TermoMin">>
                                                                                                            end, Addition_Field_list),

                                Addition_filed_element_Phones = lists:filter (   fun ({_, {struct, Add_fields_property_list}})   ->
                                                                                                                    N = proplists:get_value(<<"n">>, Add_fields_property_list),
                                                                                                                    N =:=  <<"FeedbackPhone">>
                                                                                                            end, Addition_Field_list),
                                


                                {ok, AddFieldData} = check_addition_fields (Addition_filed_element_HumidMax, Addition_filed_element_HumidMin, Addition_filed_element_TermoMax, Addition_filed_element_TermoMin, Addition_filed_element_Phones),


                                case AddFieldData of
                                    [] ->
                                        [];
                                    {HumidMax, HumidMin, TermoMax, TermoMin, Phone_list} ->
                                        #unit { 
                                                unit_id             = Unit_id, 
                                                unit_name           = Unit_name,
                                                unit_params         = #unit_params {
                                                                                humid_max   = HumidMax,
                                                                                humid_min   = HumidMin,
                                                                                termo_max   = TermoMax,
                                                                                termo_min   = TermoMin,
                                                                                phone_list  = Phone_list},
                                                last_humid_error    = #last_humid_error {error = no_error},
                                                last_termo_error    = #last_termo_error {error = no_error}
                                            }
                                        %{Unit_id, Unit_name, AddFieldData, no_error, no_error}
                                end

                            end, Item_list),

    Result_unit_list = util:remove_all_emptry_elements_from_list (Unit_list),
    {ok, Result_unit_list};
    

get_unit_list_from_wh_response ({ok, [{<<"error">>, WialonErrorCode} | _]}) ->
    {error, WialonErrorCode}.



check_addition_fields ([], _, _, _, _) ->
    {ok, []};
check_addition_fields (_, [], _, _, _) ->
    {ok, []};
check_addition_fields (_, _, [], _, _) ->
    {ok, []};
check_addition_fields (_, _, _, [], _) ->
    {ok, []};
check_addition_fields (_, _, _, _, []) ->
    {ok, []};
check_addition_fields (HumidMax, HumidMin, TermoMax, TermoMin, Phones) ->
    [{_, {struct, [{_id,_}, {_n,_}, {_v, HumidMax_value}]}} |_] = HumidMax,
    [{_, {struct, [{_id,_}, {_n,_}, {_v, HumidMin_value}]}} |_] = HumidMin,
    [{_, {struct, [{_id,_}, {_n,_}, {_v, TermoMax_value}]}} |_] = TermoMax,
    [{_, {struct, [{_id,_}, {_n,_}, {_v, TermoMix_value}]}} |_] = TermoMin,
    
    {ok, PhoneList} = get_phone_numbers_from_wh_add_flds (Phones),

    {ok, {util:binary_to_number (HumidMax_value), util:binary_to_number (HumidMin_value), util:binary_to_number (TermoMax_value), util:binary_to_number (TermoMix_value), PhoneList}}.



get_phone_numbers_from_wh_add_flds (Phones) ->
    
    PhoneList = lists:map ( fun ({_, {struct, [{_id,_}, {_n,_}, {_v, PhoneNumber}]}}) ->
                    binary_to_list (PhoneNumber)
                end, Phones),
    {ok, PhoneList}.



% Ищет resourse_id и report_id среди отчетов
get_wh_find_report_data (Sid, ReportName) ->

    Response = httpc:request(post, 
                    {"http://hst-api.wialon.com/wialon/ajax.html?svc=core/search_items", 
                        [], 
                        "application/x-www-form-urlencoded", 
                        "sid=" ++ Sid ++ "&params={spec:{itemsType:avl_resource,propName:\"\",propValueMask:*,sortType:\"\",propType:property},force:1,flags:8193,from:0,to:4294967295}"}, 
                    [],
                    []),

    

    BodyResponse = get_wialon_response (Response),
    find_report_by_name_from_wh_response (ReportName, BodyResponse).




find_report_by_name_from_wh_response (Sought_for_report_name, {ok, [{<<"searchSpec">>,_} ,_ ,_ ,_ ,_ , {<<"items">>, Item_list}]}) ->
        
    Resource_list = lists:map ( fun ({struct, Report_list}) -> 


                                Resource_ID = proplists:get_value(<<"id">>, Report_list),
                                
                                {struct, Report} = proplists:get_value(<<"rep">>, Report_list),

                                Mapped_reports = lists:map (fun ({_Number, {struct, [{<<"id">>, Report_id}, {<<"n">>, ReportName}, _Ct, _C]}}) ->

                                                    if
                                                        ReportName =:= Sought_for_report_name ->
                                                            Report_id;
                                                        ReportName =/= Sought_for_report_name ->
                                                            []
                                                    end
                                                    
                                            end, Report),

                                Cleared_mapped_reports = util:remove_all_emptry_elements_from_list (Mapped_reports),

                                if
                                    Cleared_mapped_reports =:= [] ->
                                        [];
                                    Cleared_mapped_reports =/= [] ->
                                        [Report_id | _] = Cleared_mapped_reports,
                                        {Resource_ID, Report_id}
                                end

                            end, Item_list),
    
    [Report_data | _] = util:remove_all_emptry_elements_from_list (Resource_list),
    {ok, Report_data};
    

find_report_by_name_from_wh_response (_Name, {ok, [{<<"error">>, WialonErrorCode} | _]}) ->
    {fail, {wialon_error, WialonErrorCode}}.


get_report_interval_tracing (Sid, ResourceId, ReportTemplateId, Default_value) ->

    %io:format("~p ~n", ["sid=" ++ Sid ++ "&params={itemId:" ++ integer_to_list (ResourceId) ++ " ,col:[" ++ integer_to_list (ReportTemplateId) ++ "]}"]),
    Response = httpc:request(post, 
                    {"http://hst-api.wialon.com/wialon/ajax.html?svc=report/get_report_data", 
                        [], 
                        "application/x-www-form-urlencoded", 
                        "sid=" ++ Sid ++ "&params={itemId:" ++ integer_to_list (ResourceId) ++ " ,col:[" ++ integer_to_list (ReportTemplateId) ++ "]}"}, 
                    [],
                    []),
    

    {ok, {{_HTTP_Version, 200, _HTTP_status_phrase}, _HTTP_header, Response_from_wialon}} = Response,

    [{struct, Data} | _] = mochijson2:decode (Response_from_wialon),
    {<<"tbl">>, Tables} = lists:nth(5, Data), 


    find_tracing_interval_value_by_table_name (Tables, <<"unit_sensors_tracing">>, Default_value).




find_tracing_interval_value_by_table_name ([{struct, [{<<"n">>, Next_table_name} | Fields_Tail]} | _Tail], Table_name, _Default_value) when Next_table_name =:= Table_name ->
    {<<"p">>, Prop} = lists:nth(6, Fields_Tail),
    {struct, [{<<"interval">>, {struct, [{<<"value">>, Tracing_interval_value} | _]}} | _]} = mochijson2:decode (Prop),

    {ok, Tracing_interval_value * 60 * 1000};

find_tracing_interval_value_by_table_name ([{struct, [{<<"n">>, Next_table_name} | _Fields_Tail]} | Tail], Table_name, Default_value) when Next_table_name =/= Table_name ->
    find_tracing_interval_value_by_table_name (Tail, Table_name, Default_value);

find_tracing_interval_value_by_table_name ([], _Table_name, Default_value) ->
    {ok, Default_value}.





exec_wh_report (Sid, Resource_id, Report_id, Unit_id, Date_start, Date_finish) ->
    %io:format("~p ~n", [{Sid, Resource_id, Report_id, Unit_id, Date_start, Date_finish}]),


    Response = httpc:request(post,  
                    {"http://hst-api.wialon.com/wialon/ajax.html?svc=report/exec_report", 
                        [], 
                        "application/x-www-form-urlencoded", 
                        "sid=" ++ Sid ++ "&params={reportResourceId:" ++ integer_to_list (Resource_id) ++ ", reportTemplateId:" ++ integer_to_list (Report_id) ++ ", reportObjectId:" ++ integer_to_list (Unit_id) ++ ", reportObjectSecId:0, interval:{flags:16777216, from:" ++ integer_to_list (Date_start) ++ ", to:" ++ integer_to_list (Date_finish) ++ "}}"}, 
                    [],
                    []),



    %io:format("~p ~n", ["sid=" ++ Sid ++ "&params={reportResourceId:" ++ integer_to_list (Resource_id) ++ ", reportTemplateId:" ++ integer_to_list (Report_id) ++ ", reportObjectId:" ++ integer_to_list (Unit_id) ++ ", reportObjectSecId:0, interval:{flags:16777216, from:" ++ integer_to_list (Date_start) ++ ", to:" ++ integer_to_list (Date_finish) ++ "}}"]),

    
    {ok, [{<<"reportResult">>, {struct, [{<<"msgsRendered">>,_} | Report_data]}} | _]} = get_wialon_response (Response),
    Report_data.


exec_wh_report_batch (Sid, Resource_id, Report_id, Unit_id, Date_start, Date_finish) ->

    %io:format("~p ~n", [{integer_to_list (Date_start), integer_to_list(Date_finish)}]),
    Response = httpc:request(post,  
                    {"http://hst-api.wialon.com/wialon/ajax.html?svc=core/batch", 
                        [], 
                        "application/x-www-form-urlencoded", 
                        "sid=" ++ Sid ++ "&params=[{svc:\"report/cleanup_result\",params:{}},{svc:\"report/exec_report\",params:{reportResourceId:" ++ integer_to_list (Resource_id) ++ ",reportTemplateId:" ++ integer_to_list (Report_id) ++ ", reportObjectId:" ++ integer_to_list (Unit_id) ++ ",reportObjectSecId:0,interval:{flags:16777216,from:" ++ integer_to_list (Date_start) ++ ",to:" ++ integer_to_list (Date_finish) ++ "}}}]"}, 
                    [],
                    []),

    case process_wh_response (Sid, Response) of
        {error, socket_closed_remotely} ->
            io:format("~p ~n", [{"restart", integer_to_list (Date_start), integer_to_list(Date_finish)}]),
            exec_wh_report_batch (Sid, Resource_id, Report_id, Unit_id, Date_start, Date_finish);
        {ok, Report_data} ->
            Report_data
    end.
    
    


process_wh_response (Sid, {ok, {{_HTTP_Version, 200, _HTTP_status_phrase}, _HTTP_header, Response_from_wialon}}) ->
    {struct, [{_, {struct, [_, _, {_, Tables}, _]}}]} = lists:last(mochijson2:decode (Response_from_wialon)),

    get_report_rows (Sid, Tables);


process_wh_response (_Sid, {error, socket_closed_remotely}) ->
    {error, socket_closed_remotely}.



get_report_rows (_Sid, []) ->
    {ok, []};
get_report_rows (Sid, [{struct, [_Name, _Lable, _Grouping, _Flags, {_, Rows}, _Level, _Columns, _Header]}] = Stucture) ->

    Response = httpc:request(post,  
                    {"http://hst-api.wialon.com/wialon/ajax.html?svc=report/select_result_rows", 
                        [], 
                        "application/x-www-form-urlencoded", 
                        "sid=" ++ Sid ++ "&params={tableIndex:0,config:{type:range,data:{from:0,to:" ++ integer_to_list (Rows) ++ ",level:0}}}"}, 
                    [],
                    []),

    case Response of
        {error, socket_closed_remotely} ->
            get_report_rows (Sid, Stucture);
        {ok, {{_HTTP_Version, 200, _HTTP_status_phrase}, _HTTP_header, Response_from_wialon}} ->
            Json_data = mochijson2:decode (Response_from_wialon),
            {ok, Json_data}
    end.

    

    

get_report_termo_2_tracing_list ([{struct, [_n, _i1, _i2, _t1, _t2, _d, {_c, [Date | [Humid | [Termo | _] ]]}]} | Tail], List_of_trasing_data)  ->
    Tracing = #tracing {date = binary_to_list (Date), humid = util:binary_to_number (Humid), termo = util:binary_to_number (Termo)},
    Concat_array = lists:merge (List_of_trasing_data, [Tracing]),
    get_report_termo_2_tracing_list (Tail, Concat_array);

get_report_termo_2_tracing_list ([], List_of_trasing_data) ->
    {ok, List_of_trasing_data}.




register_fuell (Sid, Date, Volume, Cost, Location, Deviation, X, Y, Desctiption, Unit_id) ->

    %io:format("~p ~n", [{Unit_id, Volume}]),
    Response = httpc:request(post,  
                    {"http://hst-api.wialon.com/wialon/ajax.html?svc=unit/registry_fuel_filling_event", 
                        [], 
                        "application/x-www-form-urlencoded", 
                        "sid=" ++ Sid ++ "&params={date:" ++ mochinum:digits (Date) ++ ", volume:" ++ mochinum:digits (Volume) ++ ", cost:" ++ mochinum:digits (Cost) ++ ",location:\"" ++ Location ++ "\",deviation:" ++ mochinum:digits (Deviation) ++ ",x:" ++ mochinum:digits (X) ++ ", y:" ++ mochinum:digits (Y) ++ ",description:\"" ++ Desctiption ++ "\",itemId: " ++ mochinum:digits (Unit_id) ++ "}"}, 
                    [],
                    []),


    io:format("~p ~n", ["sid=" ++ Sid ++ "&params={date:" ++ mochinum:digits (Date) ++ ", volume:" ++ mochinum:digits (Volume) ++ ", cost:" ++ mochinum:digits (Cost) ++ ",location:\"" ++ Location ++ "\",deviation:" ++ mochinum:digits (Deviation) ++ ",x:" ++ mochinum:digits (X) ++ ", y:" ++ mochinum:digits (Y) ++ ",description:\"" ++ Desctiption ++ "\",itemId: " ++ mochinum:digits (Unit_id) ++ "}"]),

    T = get_wialon_response (Response),
    io:format("~p ~n", [T]).



