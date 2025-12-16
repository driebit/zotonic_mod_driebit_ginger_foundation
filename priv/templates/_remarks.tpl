{% with
    remark_page|default:q.remark_page|default:1|to_integer|escape,
    remark_page_length|default:q.remark_page_length|default:20|to_integer|escape,
    show_form|default:false,
    order|default:"desc"
   as
    page,
    page_length,
    show_form,
    order
%}
    <div class="remarks do_remarks_widget" id="remarks" data-show_form="{{ show_form }}">
        {% live template="remark/remark-list.tpl"
                id=id
                topic={subject id=id predicate="about"}
                order=order
                page=page
                page_length=page_length
        %}
    </div>

    {% block new_remark_wire %}
        {% wire name="new_remark"
                action={insert_before
                    target="new-remark-link"
                    template="remark/remark-wrapper.tpl"
                    editing
                    is_new
                    id=id
                }
        %}
    {% endblock %}

    {% javascript %}
        {% if show_form %}
            z_event('new_remark');
        {% endif %}
    {% endjavascript %}

{% endwith %}
