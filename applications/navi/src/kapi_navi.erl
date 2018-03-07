%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voyager Internet Ltd.
%%% @doc
%%% Used to send push notification requests from other apps to navi.
%%% @end
%%% @contributors
%%%     Ben Partridge
%%%-------------------------------------------------------------------
-module(kapi_navi).

-export([push_device/1, push_device_v/1
        ,push_user/1, push_user_v/1
        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,publish_push_device/1, publish_push_device/2
        ,publish_push_user/1, publish_push_user/2
        ]).

-include("navi.hrl").


-define(EVENT_CATEGORY, <<"navi">>).
-define(KEY_NAVI_PUSH_DEVICE, <<"navi.push_device">>).
-define(KEY_NAVI_PUSH_USER, <<"navi.push_user">>).
-define(SHARED_HEADERS, [<<"Account-ID">>
                        ,<<"Message">>
                        ,<<"Push-Topic">>
                        ]).
-define(SHARED_TYPES, [{<<"Account-ID">>, fun is_binary/1}
                      ,{<<"Message">>, fun is_binary/1}
                      ,{<<"Metadata">>, fun kz_json:is_json_object/1}
                      ,{<<"Push-Topic">>, fun is_binary/1}
                      ]).

%% AMQP fields for Navi Push notification (to device)
%% This request should be used when you want to push a notification to a single device
%% Account-ID    = the account ID of the user to send the notification to
%% User-ID       = the ID of the user to send the notification to
%% Device-ID     = the ID of the device to send the notification to
%% Message       = the body of the push notification message to send
%% Push-Topic    = the topic/event name of the push notifcation (e.g. chat)
-define(NAVI_PUSH_DEVICE_HEADERS, [<<"Device-ID">>|?SHARED_HEADERS]).
-define(OPTIONAL_NAVI_PUSH_DEVICE_HEADERS, [<<"Metadata">>]).
-define(NAVI_PUSH_DEVICE_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                                 ,{<<"Event-Name">>, <<"push_device">>}
                                 ]).
-define(NAVI_PUSH_DEVICE_TYPES, [{<<"Device-ID">>, fun is_binary/1}|?SHARED_TYPES]).

%% AMQP fields for Navi Push notification (to user)
%% This request should be used when you want to push a notification to all devices owned by a user
%% Account-ID    = the account ID of the user to send the notification to
%% User-ID       = the ID of the user to send the notification to
%% Device-ID     = the ID of the device to send the notification to
%% Message       = the body of the push notification message to send
%% Push-Topic    = the topic/event name of the push notifcation (e.g. chat)
-define(NAVI_PUSH_USER_HEADERS, [<<"User-ID">>|?SHARED_HEADERS]).
-define(OPTIONAL_NAVI_PUSH_USER_HEADERS, [<<"Metadata">>]).
-define(NAVI_PUSH_USER_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY} ,{<<"Event-Name">>, <<"push_user">>} ]).
-define(NAVI_PUSH_USER_TYPES, [{<<"User-ID">>, fun is_binary/1}|?SHARED_TYPES]).

%%--------------------------------------------------------------------
%% @doc Authorization Request - see wiki (push device)
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec push_device(kz_term:api_terms()) ->
                         {'ok', iolist()} |
                         {'error', string()}.
push_device(Prop) when is_list(Prop) ->
    case push_device_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?NAVI_PUSH_DEVICE_HEADERS, ?OPTIONAL_NAVI_PUSH_DEVICE_HEADERS);
        'false' -> {'error', "Proplist failed validation for navi_push_device"}
    end;
push_device(JObj) ->
    push_device(kz_json:to_proplist(JObj)).

-spec push_device_v(kz_term:api_terms()) -> boolean().
push_device_v(Prop) when is_list(Prop) ->
    lager:debug("Validating payload: ~p", [Prop]),
    kz_api:validate(Prop, ?NAVI_PUSH_DEVICE_HEADERS, ?NAVI_PUSH_DEVICE_VALUES, ?NAVI_PUSH_DEVICE_TYPES);
push_device_v(JObj) ->
    push_device_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for navi gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

bind_to_q(Q, 'undefined') ->
    'ok' = amqp_util:bind_q_to_kapps(Q, ?KEY_NAVI_PUSH_USER),
    'ok' = amqp_util:bind_q_to_kapps(Q, ?KEY_NAVI_PUSH_DEVICE);
bind_to_q(Q, ['push_device'|T]) ->
    'ok' = amqp_util:bind_q_to_kapps(Q, ?KEY_NAVI_PUSH_DEVICE),
    bind_to_q(Q, T);
bind_to_q(Q, ['push_user'|T]) ->
    'ok' = amqp_util:bind_q_to_kapps(Q, ?KEY_NAVI_PUSH_USER),
    bind_to_q(Q, T);
bind_to_q(Q, [_|T]) ->
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value('restrict_to', Props)).

unbind_q_from(Q, 'undefined') ->
    'ok' = amqp_util:bind_q_to_kapps(Q, ?KEY_NAVI_PUSH_USER),
    'ok' = amqp_util:unbind_q_from_kapps(Q, ?KEY_NAVI_PUSH_DEVICE);
unbind_q_from(Q, ['push_device'|T]) ->
    'ok' = amqp_util:unbind_q_from_kapps(Q, ?KEY_NAVI_PUSH_DEVICE),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['push_user'|T]) ->
    'ok' = amqp_util:unbind_q_from_kapps(Q, ?KEY_NAVI_PUSH_USER),
    unbind_q_from(Q, T);
unbind_q_from(Q, [_|T]) ->
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) ->
    'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:kapps_exchange().

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_push_device(kz_term:api_terms()) -> 'ok'.
-spec publish_push_device(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_push_device(Props) when is_list(Props) ->
    publish_push_device(kz_json:from_list(Props));
publish_push_device(JObj) ->
    publish_push_device(JObj, ?DEFAULT_CONTENT_TYPE).
publish_push_device(Req, ContentType) ->
    lager:debug("Received request to push_device notification with navi: ~s", [kz_json:encode(Req)]),
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?NAVI_PUSH_DEVICE_VALUES, fun push_device/1),
    amqp_util:kapps_publish(?KEY_NAVI_PUSH_DEVICE, Payload, ContentType).


%%--------------------------------------------------------------------
%% @doc Authorization Request - see wiki (push user)
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec push_user(kz_term:api_terms()) ->
                         {'ok', iolist()} |
                         {'error', string()}.
push_user(Prop) when is_list(Prop) ->
    case push_user_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?NAVI_PUSH_USER_HEADERS, ?OPTIONAL_NAVI_PUSH_USER_HEADERS);
        'false' -> {'error', "Proplist failed validation for navi_push_user"}
    end;
push_user(JObj) ->
    push_user(kz_json:to_proplist(JObj)).

-spec push_user_v(kz_term:api_terms()) -> boolean().
push_user_v(Prop) when is_list(Prop) ->
    lager:debug("Validating payload: ~p", [Prop]),
    kz_api:validate(Prop, ?NAVI_PUSH_USER_HEADERS, ?NAVI_PUSH_USER_VALUES, ?NAVI_PUSH_USER_TYPES);
push_user_v(JObj) ->
    push_user_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange (push user)
%% @end
%%--------------------------------------------------------------------
-spec publish_push_user(kz_term:api_terms()) -> 'ok'.
-spec publish_push_user(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_push_user(Props) when is_list(Props) ->
    publish_push_user(kz_json:from_list(Props));
publish_push_user(JObj) ->
    publish_push_user(JObj, ?DEFAULT_CONTENT_TYPE).
publish_push_user(Req, ContentType) ->
    lager:debug("Received request to push_user notification with navi: ~s", [kz_json:encode(Req)]),
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?NAVI_PUSH_USER_VALUES, fun push_user/1),
    amqp_util:kapps_publish(?KEY_NAVI_PUSH_USER, Payload, ContentType).