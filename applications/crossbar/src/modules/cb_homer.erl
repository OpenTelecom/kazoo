%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2018, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%%
%%%
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_homer).

-export([init/0
        ,allowed_methods/2
        ,resource_exists/2
        ,validate/3
        ]).

-include("crossbar.hrl").

%% Path tokens
-define(CALL, <<"call">>).
-define(REGISTRATION, <<"registration">>).

%% Homer API config
-define(MOD_CONFIG_CAT, <<"crossbar.homer">>).
-define(HOMER_URL, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"url">>, <<"change_me">>)).
-define(HOMER_USERNAME, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"username">>, <<"change_me">>)).
-define(HOMER_PASSWORD, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"password">>, <<"change_me">>)).
-define(HOMER_SESSION_ID, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"homer_sessid">>, <<"change_me">>)).
%% Default to three days
-define(HOMER_RETENTION_HOURS, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"retention_hours">>, 3 * 24)).
-define(HOMER_TIMEZONE, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"timezone">>, <<"Etc/UTC">>)).

-type homer_transaction() :: 'call' | 'registration'.
-type homer_type() :: 'data' | 'message'.
-type homer_response() :: {'ok', kz_json:term()} | {'error', atom()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.homer">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.homer">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.homer">>, ?MODULE, 'validate'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?CALL, _CallId) -> [?HTTP_GET];
allowed_methods(?REGISTRATION, _FromId) -> [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource
%%
%% For example:
%%
%% ```
%%    /homer => [].
%%    /homer/foo => [<<"foo">>]
%%    /homer/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists(path_token(), path_token()) -> 'true' | 'false'.
resource_exists(?CALL, _) -> 'true';
resource_exists(?REGISTRATION, _) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /homer mights load a list of skel objects
%% /homer/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?CALL, CallId) ->
    validate_call(Context, CallId, cb_context:req_verb(Context));
validate(Context, ?REGISTRATION, FromId) ->
    validate_registration(Context, FromId, cb_context:req_verb(Context)).

-spec validate_call(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
validate_call(Context, CallId, ?HTTP_GET) ->
    case call_belongs_to_account(CallId, cb_context:account_id(Context)) of
        'true' -> get_call(Context, CallId);
        'false' -> cb_context:add_system_error('not_found', Context)
    end.

-spec validate_registration(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
validate_registration(Context, FromId, ?HTTP_GET) ->
    Realm = kzd_accounts:realm(cb_context:account_doc(Context)),
    Body = kz_json:from_list([{<<"from_user">>, FromId}
                             ,{<<"ruri">>, <<"%", Realm/binary, "%">>}
                             ]),
    case homer_search('registration', Body, 'data') of
        {'error', _} -> crossbar_util:response('error', <<"homer error">>, 500, 'undefined', Context);
        {'ok', []} -> cb_context:add_system_error('not_found', Context);
        {'ok', RegInfo} -> crossbar_util:response(RegInfo, Context)
    end.

-spec get_call(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
get_call(Context, CallId) ->
    %% This isn't super secure as there could be multiple calls for the same ID. At some point we
    %% should fix this
    SearchParams = kz_json:from_list([{<<"callid">>, CallId}]),
    case homer_search('call', SearchParams, 'message') of
        {'error', _} -> crossbar_util:response('error', <<"homer error">>, 500, 'undefined', Context);
        {'ok', CallInfo} -> crossbar_util:response(CallInfo, Context)
    end.

-spec call_belongs_to_account(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
call_belongs_to_account(CallId, AccountId) ->
    {{Year, Month, _}, _} = calendar:universal_time(),
    %% We only store three days of Homer data, so at worst we only need to go back one month
    {PrevYear, PrevMonth} = case Month =:= 1 of
                                'true' -> {Year-1, 12};
                                'false' -> {Year, Month-1}
                            end,
    is_call_in_modb(CallId, AccountId, Year, Month)
        orelse is_call_in_modb(CallId, AccountId, PrevYear, PrevMonth).

-spec is_call_in_modb(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> boolean().
is_call_in_modb(CallId, AccountId, Year, Month) ->
    Modb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    BinYear = kz_term:to_binary(Year),
    BinMonth = kz_date:pad_month(Month),
    DocId = <<BinYear/binary, BinMonth/binary, "-", CallId/binary>>,
    case kz_datamgr:open_cache_doc(Modb, DocId) of
        {'ok', _} -> 'true';
        _ -> 'false'
    end.

-spec homer_search(homer_transaction(), kz_json:object(), homer_type()) -> homer_response().
homer_search(Transaction, SearchParams, Type) ->
    %% We only want one transaction to be true
    BaseTransaction = kz_json:from_list([{<<"call">>, 'false'}
                                        ,{<<"registration">>, 'false'}
                                        ,{<<"rest">>, 'false'}
                                        ]),
    TransactionJObj = kz_json:set_value(kz_term:to_binary(Transaction), 'true', BaseTransaction),
    Body = kz_json:from_list([{<<"timestamp">>, homer_period()}
                             ,{<<"transaction">>, TransactionJObj}
                             ,{<<"param">>, kz_json:from_list([{<<"search">>, SearchParams}])}
                             ]),
    Endpoint = <<"search/", (kz_term:to_binary(Type))/binary>>,
    lager:debug("searching homer endpoint ~s with body: ~s", [Endpoint, kz_json:encode(Body)]),
    homer_authed_request(Endpoint, Body).

-spec homer_authed_request(kz_term:ne_binary(), kz_json:object()) -> homer_response().
homer_authed_request(Endpoint, Body) ->
    case homer_request(Endpoint, Body) of
        {'error', 'noauth'} ->
            _ = authenticate(),
            homer_request(Endpoint, Body);
        Resp ->
            Resp
    end.

-spec authenticate() -> homer_response().
authenticate() ->
    lager:debug("reauthenticating with homer"),
    Body = kz_json:from_list([{<<"username">>, ?HOMER_USERNAME}
                             ,{<<"password">>, ?HOMER_PASSWORD}
                             ]),
    homer_request(<<"session">>, Body).

-spec homer_request(kz_term:ne_binary(), kz_json:object()) -> homer_response().
homer_request(Endpoint, Body) ->
    URL = string:strip(kz_term:to_list(?HOMER_URL), 'right', $/) ++ "/" ++ kz_term:to_list(Endpoint),
    %% Potentially ensure, I don't think we should always be using the same cookie
    Cookie = kz_term:to_list(<<"HOMERSESSID=", (?HOMER_SESSION_ID)/binary, "; path=/">>),

    lager:debug("making homer request to url ~s with content: ~s", [URL, kz_json:encode(Body)]),
    case kz_http:post(URL, [{"Content-Type", "application/json"}, {"Cookie", Cookie}], kz_json:encode(Body)) of
        {'ok', 200, _, Response} ->
            lager:debug("homer response: ~s", [Response]),
            JObj = kz_json:decode(Response),
            {'ok', kz_json:get_value(<<"data">>, JObj)};
        {'ok', 403, _, _} ->
            lager:warning("homer auth failed"),
            {'error', 'noauth'};
        {'ok', Code, _, Response} ->
            lager:debug("failed with response code: ~p data: ~p", [Code, Response]),
            {'error', 'badrespcode'};
        {'error', 'no_scheme'} ->
            lager:warning("homer URL had no scheme (http/https). probably not configured"),
            {'error', 'noconnect'};
        Err ->
            lager:warning("failed to contact homer with error: ~p", [Err]),
            {'error', 'noconnect'}
    end.

-spec homer_period() -> kz_json:object().
homer_period() ->
    LocalTime = localtime:utc_to_local(calendar:universal_time(), kz_term:to_list(?HOMER_TIMEZONE)),
    To = kz_time:gregorian_seconds_to_unix_seconds(calendar:datetime_to_gregorian_seconds(LocalTime)),
    %% Go back to start of retention period
    From = To - (?HOMER_RETENTION_HOURS * ?SECONDS_IN_HOUR),
    %% There seems to be a bug in Homer, meaning we need the +1 to work
    kz_json:from_list([{<<"from">>, From * ?MILLISECONDS_IN_SECOND}
                      ,{<<"to">>, (To + 1) * ?MILLISECONDS_IN_SECOND}
                      ]).
