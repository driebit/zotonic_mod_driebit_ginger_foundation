-module(m_driebit_ginger_foundation).

-export([ m_get/3 ]).

m_get([ <<"recaptcha_key">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(site, recaptcha_key, Context), Rest}}.
