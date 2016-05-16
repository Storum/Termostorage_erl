-module(util).
-author('storumproject@gmail.com').
 -include("records.hrl").


-export([
            get_ev/3, 
            get_ev/2, 
            start_apps/1, 
            stop_apps/1, 
            open_dets_file/1, 
            parse_csv_file/1, 
            remove_all_emptry_elements_from_list/1, 
            get_current_unixtime/0,
            binary_to_number/1, 
            float_to_list_ext/1, 
            connect/0, 
            date_to_timestamp/1, 
            date_time_to_string/1, 
            convert_wh_date/1, 
            send_mail2sms/2,
            reload_list/2]).


get_ev (AppName, Ev, Def) ->
    case application:get_env (AppName, Ev) of
        {ok, Value} ->
            Value;
        undefined ->
            Def
    end.

get_ev (Ev, Def) ->
    case application:get_env (Ev) of
        {ok, Value} ->
            Value;
        undefined ->
            Def
    end.


start_apps ([]) -> ok;
start_apps([App | Apps]) ->
    case application:start(App) of
        ok -> start_apps(Apps);
        {error, {already_started, App}} -> start_apps(Apps);
        Other->io:format("~p ~n", [Other])
    end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
    application:stop(App),
    stop_apps(Apps).






open_dets_file (Dets_file_name) ->
    dets:open_file (Dets_file_name, [{auto_save, 180000}, {type, duplicate_bag}, {file, Dets_file_name}]).







parse_csv_file (File_name) ->
  {ok, Data} = file:read_file (File_name),
  parse_csv_lines (Data).
 
parse_csv_lines (Data) ->
    Lines = re:split (Data, "\r|\n|\r\n", []), 
    
    [ 
        [
            Token
            || 

            Token <- re:split (Line, ";", []) 
        ]

        || 

        Line <- Lines,
        Line =/= <<"">>
    ].




remove_all_emptry_elements_from_list (List) ->
    lists:filtermap ( fun (Element) ->
                        case Element of
                            [] -> false;
                            _Elem -> {true, Element}
                        end
                    end, List).



get_current_unixtime () ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600.


binary_to_number (B) ->
    list_to_number (binary_to_list(B)).

list_to_number (L) ->
    try list_to_float (L)
    catch
        error:badarg ->
            list_to_integer (L)
    end.


float_to_list_ext (Value) when is_float (Value)->
    float_to_list (Value);

float_to_list_ext (Value) when is_integer (Value)->
    integer_to_list (Value).



date_to_timestamp (DateTime) ->
    Seconds = calendar:datetime_to_gregorian_seconds (DateTime) - 62167219200,
    Seconds.


date_time_to_string ({{Year, Month, Day}, {Hour, Minute, Seconds}}) ->

    Year_ = convert_number_to_string_with_leading_zero (Year),
    Month_ = convert_number_to_string_with_leading_zero (Month),
    Day_ = convert_number_to_string_with_leading_zero (Day),
    Hour_ = convert_number_to_string_with_leading_zero (Hour),
    Minute_ = convert_number_to_string_with_leading_zero (Minute),
    _Seconds_ = convert_number_to_string_with_leading_zero (Seconds),

    Hour_ ++ ":" ++ Minute_ ++ " " ++ Day_ ++ "." ++ Month_ ++ "." ++ Year_.
    %Day_ ++ "." ++ Month_ ++ "." ++ Year_ ++ " " ++ Hour_ ++ ":" ++ Minute_ .%++ ":" ++ Seconds_.



convert_number_to_string_with_leading_zero (Number) ->
    if
        Number < 10 ->
            "0" ++ integer_to_list (Number);
        true ->
            integer_to_list (Number)
    end.



convert_wh_date (Wh_date) ->
    DateTime = ec_date:parse(Wh_date),
    %Local_time = calendar:universal_time_to_local_time (DateTime),
    Local_time = localtime:utc_to_local(DateTime, "Europe/Moscow"),

    date_time_to_string (Local_time).




connect() ->
    {ok, Socket} = ssl:connect("smtp.yandex.ru", 465, [{active, false}], 1000),
    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode("mail@termostorage.ru"))),
    send(Socket, binary_to_list(base64:encode("59JArLIZXoDKl583"))),
    send(Socket, "MAIL FROM: <mail@termostorage.ru>"),
    send(Socket, "RCPT TO:<mail2sms@mcommunicator.ru>"),
    send(Socket, "DATA"),
    send_no_receive(Socket, "Content-Type: text/plain;charset=utf-8"),
    send_no_receive(Socket, "From: <mail@termostorage.ru>"),
    send_no_receive(Socket, "To: <mail2sms@mcommunicator.ru>"),
    send_no_receive(Socket, "Date: Tue, 15 Jan 2008 16:02:43 +0000"),
    send_no_receive(Socket, "Subject: @79892777067"),
    send_no_receive(Socket, ""),
    send_no_receive(Socket, "Привет2"),
    send_no_receive(Socket, ""),
    send(Socket, "."),
    send(Socket, "QUIT"),
    ssl:close(Socket).

send_no_receive(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n").


send(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).

recv(Socket) ->
    case ssl:recv(Socket, 0, 1000) of
 {ok, Return} -> io:format("~p~n", [Return]);
 {error, Reason} -> io:format("ERROR: ~p~n", [Reason])
    end.

    

send_mail2sms ([NextPhone | Tail], Text) ->
    Body = unicode:characters_to_binary("Content-Type: text/plain;charset=utf-8\r\nSubject: @" ++ NextPhone ++ "\r\nFrom: <mail@termostorage.ru>\r\n\To: <mail2sms@mcommunicator.ru>r\n\r\n " ++ Text),
    gen_smtp_client:send({"mail@termostorage.ru", ["mail2sms@mcommunicator.ru"], Body}, [{relay, "smtp.yandex.ru"}, {port, 465}, {username, "mail@termostorage.ru"},{password, "59JArLIZXoDKl583"}, {ssl, true}]),

    send_mail2sms (Tail, Text);


send_mail2sms ([], _Text) ->
    ok.



% проверяет наличие Verifiable_unit в списке
check_unit_presence_in_list (#unit{} = Verifiable_unit, [#unit{} = Next_old_unit | _Tail]) when Verifiable_unit#unit.unit_id =:= Next_old_unit#unit.unit_id ->
    {ok, is_present, Verifiable_unit, Next_old_unit};

check_unit_presence_in_list (#unit{} = Verifiable_unit, [#unit{} = Next_old_unit | Tail]) when Verifiable_unit#unit.unit_id =/= Next_old_unit#unit.unit_id ->
    check_unit_presence_in_list (Verifiable_unit, Tail);

check_unit_presence_in_list (#unit{} = Verifiable_unit, []) ->
    {ok, is_not_present, Verifiable_unit}.



% обновляют поля нового unit-а, который присутсвует в старом списке новыми полями, кроме полей ошибок
% поля ошибос остаются от старого unit-а
update_unit_fields ({ok, is_present, #unit{} = New_unit, #unit{} = Original_unit}) ->
    {ok, [#unit {
            unit_id = New_unit#unit.unit_id,
            unit_name = New_unit#unit.unit_name,
            unit_params = New_unit#unit.unit_params,
            last_humid_error = Original_unit#unit.last_humid_error,
            last_termo_error = Original_unit#unit.last_termo_error
            }]};

% если новый unit не присутствует в старом списке, просто оставляем его как есть
update_unit_fields ({ok, is_not_present, #unit{} = New_unit}) ->
    {ok, [New_unit]}.



% обновляет новые unit-ы, которые присутствуют в старом списке
update_unit_list ([#unit{} = Next_new_unit | Tail], Old_unit_list, Result_unit_list) ->
    {ok, Result_unit} = update_unit_fields (check_unit_presence_in_list (Next_new_unit, Old_unit_list)),
    New_result_unit_list = lists:append (Result_unit_list, Result_unit),

    update_unit_list (Tail, Old_unit_list, New_result_unit_list);

update_unit_list ([], _Old_unit_list, Result_unit_list) ->
    {ok, Result_unit_list}.



% возвращает unit если он присуствует или пустой списк если отсуствует
stay_if_present ({ok, is_present, Verifiable_unit, _Next_old_unit}) ->
    {ok, [Verifiable_unit]};

stay_if_present ({ok, is_not_present, _Verifiable_unit}) ->
    {ok, []}.



% удаляет старые unit-ы если их нет в новом списке
delete_old_deleted_unit ([#unit{} = Next_old_unit | Tail], New_unit_list, Result_unit_list) ->
    {ok, Result_unit} = stay_if_present (check_unit_presence_in_list (Next_old_unit, New_unit_list)),
    New_result_unit_list = lists:append (Result_unit_list, Result_unit),

    delete_old_deleted_unit (Tail, New_unit_list, New_result_unit_list);

delete_old_deleted_unit ([], _New_unit_list, Result_unit_list) ->
    {ok, Result_unit_list}.



% полностью обновляет список unit-ов
reload_list (New_unit_list, Old_unit_list) ->
    {ok, Cleared_from_deleted_unit_list} = delete_old_deleted_unit (Old_unit_list, New_unit_list, []),
    update_unit_list (New_unit_list, Cleared_from_deleted_unit_list, []).








