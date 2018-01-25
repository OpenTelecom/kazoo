-module(kazoo_tts_aws_polly).

-behaviour(gen_tts_provider).

-export([create/4
    , set_api_key/1
    , check_aws_config/0
    , get_aws_config/0
    , check_voice/0
    , set_aws_key_id/1
    , set_aws_secret_key/1]).

-include("kazoo_speech.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("lhttpc/include/lhttpc.hrl").

%%{
%%    "data": {
%%        "default": {
%%            "tts_url_aws_polly": "https://polly.eu-west-2.amazonaws.com/v1/speech",
%%            "tts_aws_key_id": "AKIAIFHXKQEEHHMCT77Q",
%%            "tts_aws_secret_key": "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"
%%        },
%%        "id": "speech"
%%    }
%%}

-define(ISPEECH_VOICE_MAPPINGS
       ,[{<<"female/en-us">>, <<"usenglishfemale">>}
        ,{<<"male/en-us">>, <<"usenglishmale">>}
        ,{<<"female/en-ca">>, <<"caenglishfemale">>}
        ,{<<"female/en-au">>, <<"auenglishfemale">>}
        ,{<<"female/en-gb">>, <<"ukenglishfemale">>}
        ,{<<"male/en-gb">>, <<"ukenglishmale">>}
        ,{<<"female/es-us">>, <<"usspanishfemale">>}
        ,{<<"male/es-us">>, <<"usspanishmale">>}
        ,{<<"female/us-us">>, <<"usspanishfemale">>}
        ,{<<"female/zh-cn">>, <<"chchinesefemale">>}
        ,{<<"male/zh-cn">>, <<"chchinesemale">>}
        ,{<<"female/zh-hk">>, <<"hkchinesefemale">>}
        ,{<<"female/zh-tw">>, <<"twchinesefemale">>}
        ,{<<"female/ja-jp">>, <<"jpjapanesefemale">>}
        ,{<<"male/ja-jp">>, <<"jpjapanesemale">>}
        ,{<<"female/ko-kr">>, <<"krkoreanfemale">>}
        ,{<<"male/ko-kr">>, <<"krkoreanmale">>}
        ,{<<"female/da-dk">>, <<"eurdanishfemale">>}
        ,{<<"female/de-de">>, <<"eurgermanfemale">>}
        ,{<<"male/de-de">>, <<"eurgermanmale">>}
        ,{<<"female/ca-es">>, <<"eurcatalanfemale">>}
        ,{<<"female/es-es">>, <<"eurspanishfemale">>}
        ,{<<"male/es-es">>, <<"eurspanishmale">>}
        ,{<<"female/fi-fi">>, <<"eurfinnishfemale">>}
        ,{<<"female/fr-ca">>, <<"cafrenchfemale">>}
        ,{<<"male/fr-ca">>, <<"cafrenchmale">>}
        ,{<<"female/fr-fr">>, <<"eurfrenchfemale">>}
        ,{<<"male/fr-fr">>, <<"eurfrenchmale">>}
        ,{<<"female/it-it">>, <<"euritalianfemale">>}
        ,{<<"male/it-it">>, <<"euritalianmale">>}
        ,{<<"female/nb-no">>, <<"eurnorwegianfemale">>}
        ,{<<"female/nl-nl">>, <<"eurdutchfemale">>}
        ,{<<"female/pl-pl">>, <<"eurpolishfemale">>}
        ,{<<"female/pt-br">>, <<"brportuguesefemale">>}
        ,{<<"female/pt-pt">>, <<"eurportuguesefemale">>}
        ,{<<"male/pt-pt">>, <<"eurportuguesemale">>}
        ,{<<"female/ru-ru">>, <<"rurussianfemale">>}
        ,{<<"male/ru-ru">>, <<"rurussianmale">>}
        ,{<<"female/sv-se">>, <<"swswedishfemale">>}
        ,{<<"female/hu-hu">>, <<"huhungarianfemale">>}
        ,{<<"female/cs-cz">>, <<"eurczechfemale">>}
        ,{<<"female/tr-tr">>, <<"eurturkishfemale">>}
        ,{<<"male/tr-tr">>, <<"eurturkishmale">>}
        ]
       ).

-define(ISPEECH_TTS_URL, kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_url_ispeech">>, <<"http://api.ispeech.org/api/json">>)).

-define(AWS_POLLY_SPEECH_METHOD, 'post').
-define(AWS_POLLY_VOICES_URL, <<"https://polly.eu-west-2.amazonaws.com/v1/voices">>).
-define(AWS_POLLY_VOICES_METHOD, 'get').
-define(AWS_POLLY_VOICES_LANGUAGE_CODE_MAP, #{<<"LanguageCode">> => <<"en-GB">>}).


-spec set_api_key(kz_term:ne_binary()) -> 'ok'.
set_api_key(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_api_key">>, Key),
    'ok'.

-spec set_aws_key_id(kz_term:ne_binary()) -> 'ok'.
set_aws_key_id(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_aws_key_id">>, Key),
    'ok'.

-spec set_aws_secret_key(kz_term:ne_binary()) -> 'ok'.
set_aws_secret_key(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_aws_secret_key">>, Key),
    'ok'.

-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    VoiceMappings = ?ISPEECH_VOICE_MAPPINGS,
    case props:get_value(kz_term:to_lower_binary(Voice), VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        ISpeechVoice ->
            make_request(Text, ISpeechVoice, Format, Options)
    end.

-spec make_request(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
make_request(Text, ISpeechVoice, Format, Options) ->
    BaseUrl = ?ISPEECH_TTS_URL,

    Props = [{<<"text">>, Text}
            ,{<<"voice">>, ISpeechVoice}
            ,{<<"format">>, Format}
            ,{<<"action">>, <<"convert">>}
            ,{<<"apikey">>, ?TTS_API_KEY}
            ,{<<"speed">>, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_speed">>, 0)}
            ,{<<"startpadding">>, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_start_padding">>, 1)}
            ,{<<"endpadding">>, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_end_padding">>, 0)}
            ],
    Headers = [{"Content-Type", "application/json; charset=UTF-8"}],
    Body = kz_json:encode(kz_json:from_list(Props)),

    lager:debug("sending TTS request to ~s", [BaseUrl]),

    HTTPOptions = props:delete('receiver', Options),
    case props:get_value('receiver', Options) of
        Pid when is_pid(Pid) ->
            Response = kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions),
            create_response(Response);
        _ ->
            Response = kz_http:post(BaseUrl, Headers, Body, HTTPOptions),
            create_response(Response)
    end.

-spec create_response(kz_http:ret()) ->
                             kz_http:req_id() |
                             {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                             {'error', 'tts_provider_failure', binary()}.
create_response({'error', _R}) ->
    lager:warning("creating speech file failed with error ~p", [_R]),
    {'error', 'tts_provider_failure', <<"unexpected error encountered accessing provider">>};
create_response({'http_req_id', ReqID}) ->
    lager:debug("speech file streaming as ~p", [ReqID]),
    {'ok', ReqID};
create_response({'ok', 200, Headers, Content}) ->
    ContentType = props:get_value("content-type", Headers),
    ContentLength = props:get_value("content-length", Headers),
    lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
    {'ok', kz_term:to_binary(ContentType), Content};
create_response({'ok', _Code, RespHeaders, Content}) ->
    lager:warning("creating speech file failed with code ~p: ~p", [_Code, Content]),
    _ = [lager:debug("hdr: ~p", [H]) || H <- RespHeaders],
    {'error', 'tts_provider_failure', kz_json:get_value(<<"message">>, kz_json:decode(Content))}.



-spec get_aws_config() -> {string(),aws_config()}.
get_aws_config() ->
    {_ok, {_Protocol,_UserInfo, Host, _Port, _Path, _Query}} = http_uri:parse(binary_to_list(?AWS_POLLY_VOICES_URL)),
    [Service |[ Region | _ ]] = re:split(Host, "[.]",[{return,list}]),
    {
        Service,
        #aws_config{
        access_key_id = "AKIAIFHXKQEEHHMCT77Q",
        secret_access_key = "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",
        security_token = 'undefined',
        aws_region = Region,
        http_client = 'httpc'}
    }.

-spec check_aws_config() -> 'ok'.
check_aws_config() ->
    {Service, Config} = get_aws_config(),
    io:format("service=~p~n",[Service]),
    io:format("~p=~p~n",['access_key_id', Config#aws_config.access_key_id]),
    io:format("~p=~p~n",['secret_access_key', Config#aws_config.secret_access_key]),
    io:format("~p=~p~n",['aws_region', Config#aws_config.aws_region]),
    'ok'.

-spec check_voice() -> any().
check_voice() ->
    {Service, Config} = get_aws_config(),
    Method = ?AWS_POLLY_VOICES_METHOD,
    {'ok', {ProtocolAtom,_UserInfo, Host, Port, Path, _Query}} = http_uri:parse(binary_to_list(?AWS_POLLY_VOICES_URL)),
    Protocol = atom_to_list(ProtocolAtom),
    Params = maps:to_list(?AWS_POLLY_VOICES_LANGUAGE_CODE_MAP),
    Region = Config#aws_config.aws_region,
    io:format("Method=~p~nProtocol=~p~nHost=~p~nPort=~p~nPath=~p~nParams=~p~nService=~p~nRegion=~p~n",
        [Method,Protocol,Host,Port,Path,Params,Service,Region]),
    case erlcloud_aws:aws_request4(Method, Protocol, Host, Port, Path, Params, Service,
        Config) of
    {ok, RespBody} ->
        {ok, jsx:decode(RespBody)};
    {error, Reason} ->
        {error, Reason}
    end.


%%-spec get_lang_pair() -> map().
%%get_lang_pair() ->
%%  Map = ?AWS_POLLY_VOICES_LANGUAGE_CODE_MAP,
%%  [Key|_Other] = maps:keys(Map),
%%  Value = maps:get(Key,Map),
%%  io:format("Key=~p, Value=~p ~n",[Key,Value]).




