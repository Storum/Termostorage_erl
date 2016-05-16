%% -*- coding: utf-8 -*-
-author('storumproject@gmail.com').
 
%%----------------------------------------------------------------------
%%  Unit params record
%%----------------------------------------------------------------------
-record (unit_params,  {
                        humid_max, 
                        humid_min, 
                        termo_max,
                        termo_min, 
                        phone_list
                }).

%%----------------------------------------------------------------------
%%  last humid error
%%----------------------------------------------------------------------
-record (last_humid_error,  {
                        error, 
                        humid, 
                        date
                }).

%%----------------------------------------------------------------------
%%  last termo error
%%----------------------------------------------------------------------
-record (last_termo_error,  {
                        error, 
                        termo, 
                        date
                }).



%%----------------------------------------------------------------------
%%  Unit record
%%----------------------------------------------------------------------
-record (unit,  {
                    unit_id,
                    unit_name,
                    unit_params = #unit_params{},
                    last_humid_error = #last_humid_error{},
                    last_termo_error = #last_termo_error{}
                }).


%%----------------------------------------------------------------------
%%  tracing record
%%----------------------------------------------------------------------
-record (tracing,  {
                    date,
                    humid,
                    termo
                }).



%%----------------------------------------------------------------------
%%  Status of gen_server definition
%%----------------------------------------------------------------------
-record (state,  {
                    token,
                    resource_id,
                    report_template_id,
                    unit_list,
                    last_finish_time = null,
                    interval,
                    report_template_name,
                    timer
                }).



%%----------------------------------------------------------------------
%%  Params of working
%%----------------------------------------------------------------------
-record (params_of_working,  {
                    sid,
                    resource_id,
                    report_template_id,
                    interval,
                    unit_list
                }).

