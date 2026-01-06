-module(ginger_captcha).

-export([ is_valid_captcha/2 ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Validate the captcha if the recaptcha key in the submit args is the
%% same as the configured captch key. If there is no captcha key configured then
%% this returns true.
-spec is_valid_captcha(proplists:proplist(), z_context:context()) -> boolean().
is_valid_captcha(Args, Context) ->
    CaptchaKey = m_config:get_value(site, recaptcha_key, Context),
    CaptchaSecretKey = m_config:get_value(site, recaptcha_secret_key, Context),
    CaptchaKeyArg = proplists:get_value(recaptcha_key, Args),
    if
        CaptchaKey =:= undefined; CaptchaKey =:= <<>>; CaptchaKey =:= "" ->
            ?LOG_INFO(#{
                in => zotonic_driebit_ginger_foundation,
                text => <<"Validated captcha because the config site.recaptcha_key is empty">>,
                result => ok
            }),
            true;
        CaptchaKey =:= CaptchaKeyArg ->
            Token = case z_context:get_q(<<"recaptcha-token">>, Context) of
                undefined -> z_context:get_q(<<"g-recaptcha-response">>, Context);
                Tk -> Tk
            end,
            validate_captcha_token(Token, CaptchaSecretKey, Context);
        true ->
            true
    end.

validate_captcha_token(Token, _SecretKey, _Context) when Token =:= undefined; Token =:= <<>> ->
    ?LOG_ERROR(#{
        in => zotonic_driebit_ginger_foundation,
        text => <<"Error validating captcha, empty token passed.">>,
        result => error,
        reason => no_recaptcha_secret_key
    }),
    false;
validate_captcha_token(_Token, Secret, _Context) when Secret =:= undefined; Secret =:= <<>> ->
    ?LOG_ERROR(#{
        in => zotonic_driebit_ginger_foundation,
        text => <<"Error validating captcha site.recaptcha_secret_key is not configured.">>,
        result => error,
        reason => no_recaptcha_secret_key
    }),
    false;
validate_captcha_token(Token, SecretKey, Context) ->
    Url = <<"https://www.google.com/recaptcha/api/siteverify">>,
    Payload = #{
        <<"secret">> => SecretKey,
        <<"response">> => Token
    },
    case z_fetch:fetch_json(post, Url, Payload, [], Context) of
        {ok, #{ <<"success">> := Success }} when is_boolean(Success) ->
            ?LOG_INFO(#{
                in => zotonic_driebit_ginger_foundation,
                text => <<"Validated captcha">>,
                result => ok,
                success => Success
            }),
            Success;
        {ok, Resp} ->
            ?LOG_ERROR(#{
                in => zotonic_driebit_ginger_foundation,
                text => <<"Error validating captcha, unexpected return from Google API">>,
                result => error,
                reason => json,
                response => Resp
            }),
            false;
        {error, Reason} ->
            ?LOG_ERROR(#{
                in => zotonic_driebit_ginger_foundation,
                text => <<"Error validating captcha, error return from Google API">>,
                result => error,
                reason => Reason
            }),
            false
    end.