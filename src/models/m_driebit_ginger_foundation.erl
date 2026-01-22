-module(m_driebit_ginger_foundation).

-export([
    m_get/3,
    is_comments_enabled/2
]).

m_get([ <<"recaptcha_key">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(site, recaptcha_key, Context), Rest}};
m_get([ <<"is_comments_enabled">>, Id | Rest ], _Msg, Context) ->
    {ok, {is_comments_enabled(m_rsc:rid(Id, Context), Context), Rest}}.

is_comments_enabled(undefined, _Context) ->
    false;
is_comments_enabled(Id, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true ->
            CatId = m_rsc:p_no_acl(Id, <<"category_id">>, Context),
            case m_rsc:p_no_acl(CatId, <<"is_feature_enable_comments">>, Context) of
                true ->
                    case m_rsc:p_no_acl(Id, <<"comments_enabled_for">>, Context) of
                        undefined -> true;
                        <<"anonymous">> -> m_config:get_boolean(site, anonymous_comments_enabled, Context);
                        <<"user">> -> z_auth:is_auth(Context);
                        <<"none">> -> false;
                        _ -> false
                    end;
                _ ->
                    false
            end;
        false ->
            false
    end.
