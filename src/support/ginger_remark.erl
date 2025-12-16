%% @doc Support routines for remarks. Modified from Zotonic 0.x mod_ginger_remark.
%% The ginger_remark module has been incorporated into the zotonic_driebit_ginger_foundation
%% module for easier editing and deployment.

-module(ginger_remark).

-export([
    event/2,
    notify_followers/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#submit{ message = {remark_save, Args} }, Context) ->
    case ginger_captcha:is_valid_captcha(Args, Context) of
        true ->
            {body_element, BodyElement} = proplists:lookup(body_element, Args),
            {about_id, AboutId0} = proplists:lookup(about_id, Args),
            {remark_id, RemarkId0} = proplists:lookup(remark_id, Args),
            AboutId = z_convert:to_integer(AboutId0),
            RemarkId = z_convert:to_integer(RemarkId0),
            case is_body_ok(Context) of
                true ->
                    case z_acl:rsc_visible(AboutId, Context) of
                        true ->
                            case remark_save(RemarkId, AboutId, z_acl:user(Context), Context) of
                                {ok, NewRemarkId} ->
                                    ?LOG_INFO(#{
                                        in => zotonic_driebit_ginger_foundation,
                                        text => <<"Saved remark">>,
                                        result => ok,
                                        remark_id => NewRemarkId,
                                        about_id => AboutId,
                                        user_id => z_acl:user(Context)
                                    }),
                                    Script = iolist_to_binary([
                                        <<"$(document).trigger('remark:saved', ">>,
                                            integer_to_binary(NewRemarkId),
                                        <<");">>
                                    ]),
                                    Context1 = z_render:wire({script, [ {script, Script} ]}, Context),
                                    OnSuccess = proplists:get_all_values(on_success, Args),
                                    z_render:wire(OnSuccess, Context1);
                                {error, Reason} ->
                                    ?LOG_ERROR(#{
                                        in => zotonic_driebit_ginger_foundation,
                                        text => <<"Error saving remark">>,
                                        result => error,
                                        reason => Reason,
                                        remark_id => RemarkId,
                                        about_id => AboutId,
                                        user_id => z_acl:user(Context)
                                    })
                            end;
                        false ->
                            z_render:growl_error(?__("You can only remark about things you can see.", Context), Context)
                    end;
                false ->
                    % Mark the body field as error
                    z_render:wire({add_class, [ {target, BodyElement}, {class, "has-error is_error"} ]}, Context)
            end;
        false ->
            z_render:wire({fade_in, [ {target, <<"captcha-failed">>} ]}, Context)
    end.


%% @doc Notify all followers of an article that a new remark has been added.
notify_followers(AboutId, RemarkId, Context) ->
    ContextAsync = z_context:prune_for_async(Context),
    z_proc:spawn_md(fun() ->
        do_notify_followers(AboutId, RemarkId, ContextAsync)
    end).

do_notify_followers(AboutId, RemarkId, Context) ->
    Authors = m_edge:objects(AboutId, author, Context),
    Followers = m_edge:subjects(AboutId, follow, Context),
    RecipientIds = lists:usort(Authors ++ Followers),
    ContextSudo = z_acl:sudo(Context),
    lists:foreach(
        fun(RecipientId) ->
            Vars = [
                {about, AboutId},
                {remark, RemarkId},
                {person, RecipientId},
                {author, lists:member(RecipientId, Authors)},
                {recipient_id, RecipientId}
            ],
            case m_rsc:p_no_acl(RecipientId, <<"email_raw">>, Context) of
                <<>> -> ok;
                undefined -> ok;
                Email ->
                    z_email:send_render(Email, "email/email-follow.tpl", Vars, ContextSudo)
            end
        end,
        RecipientIds).

%% @doc The body must have text or an image.
is_body_ok(Context) ->
    case z_context:get_q(<<"body">>, Context) of
        undefined ->
            false;
        Body when is_binary(Body) ->
            case binary:match(Body, <<"<!-- z-media ">>) of
                nomatch ->
                    case z_string:trim(z_html:strip(Body)) of
                        <<>> -> false;
                        _ -> true
                    end;
                {_, _} ->
                    true
            end;
        _ ->
            false
    end.

%% @doc Create a new remark or update an existing remark.
remark_save(undefined, AboutId, undefined, Context) when is_integer(AboutId) ->
    % New remark from an anonymous user.
    Name = z_context:get_q_validated(<<"anonymous_name">>, Context),
    Email = z_context:get_q_validated(<<"anonymous_email">>, Context),
    IsEmailVisible = z_convert:to_bool(z_context:get_q(<<"anonymous_email_visible">>, Context)),
    Title = z_context:get_q_validated(<<"title">>, Context),
    Body = z_context:get_q(<<"body">>, Context),
    Props = #{
        <<"category_id">> => <<"remark">>,
        <<"content_group_id">> => <<"cg_user_generated">>,
        <<"is_published">> => true,
        <<"is_dependent">> => false,
        <<"rights">> => <<"CR">>,
        <<"anonymous_name">> => Name,
        <<"anonymous_email">> => Email,
        <<"anonymous_email_visible">> => IsEmailVisible,
        <<"title">> => Title,
        <<"body">> => Body
    },
    case m_rsc:insert(Props, Context) of
        {ok, RemarkId} ->
            case m_edge:insert(RemarkId, about, AboutId, z_acl:sudo(Context)) of
                {ok, _} ->
                    notify_followers(AboutId, RemarkId, z_acl:sudo(Context)),
                    {ok, RemarkId};
                {error, _} = Error ->
                    m_rsc:delete(RemarkId, z_acl:sudo(Context)),
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
remark_save(undefined, AboutId, UserId, Context) when is_integer(AboutId) ->
    % New remark from known user
    Title = z_context:get_q_validated(<<"title">>, Context),
    Body = z_context:get_q(<<"body">>, Context),
    Props = #{
        <<"category_id">> => <<"remark">>,
        <<"content_group_id">> => <<"cg_user_generated">>,
        <<"is_published">> => true,
        <<"is_dependent">> => false,
        <<"rights">> => <<"CR">>,
        <<"title">> => Title,
        <<"body">> => Body
    },
    case m_rsc:insert(Props, Context) of
        {ok, RemarkId} ->
            case m_edge:insert(RemarkId, about, AboutId, Context) of
                {ok, _} ->
                    {ok, _} = m_edge:insert(RemarkId, author, UserId, z_acl:sudo(Context)),
                    notify_followers(AboutId, RemarkId, Context),
                    {ok, RemarkId};
                {error, _} = Error ->
                    m_rsc:delete(RemarkId, z_acl:sudo(Context)),
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
remark_save(_RemarkId, _AboutId, undefined, _Context) ->
    {error, eacces};
remark_save(RemarkId, AboutId, _UserId, Context) when is_integer(AboutId) ->
    % Editing an existing remark
    Title = z_context:get_q_validated(<<"title">>, Context),
    Body = z_context:get_q(<<"body">>, Context),
    Props = #{
        <<"title">> => Title,
        <<"body">> => Body
    },
    case m_rsc:update(RemarkId, Props, Context) of
        {ok, _} ->
            Es = m_edge:objects(RemarkId, about, Context),
            case lists:member(AboutId, Es) of
                true ->
                    % Force update of the remarks list - even when no edge has been made
                    Topic = [
                        <<"model">>, <<"edge">>, <<"event">>, AboutId, <<"s">>, <<"about">>
                    ],
                    z_mqtt:publish(Topic, #{}, z_acl:sudo(Context));
                false ->
                    m_edge:insert(RemarkId, about, AboutId, Context)
            end,
            {ok, RemarkId};
        {error, _} = Error ->
            Error
    end.


