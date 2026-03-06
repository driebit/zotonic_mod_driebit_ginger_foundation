<!DOCTYPE html>
<!--
#################################
      _      _      _     _ _
   __| |_ __(_) ___| |__ (_) |_
  / _` | '__| |/ _ \ '_ \| | __|
 | (_| | |  | |  __/ |_) | | |_
  \__,_|_|  |_|\___|_.__/|_|\__|

############ driebit ############

 geavanceerde internetapplicaties

        Oudezijds Voorburgwal 282
                1012 GL Amsterdam
                   020 - 420 8449
                  info@driebit.nl
                   www.driebit.nl

#################################
//-->
<html lang="{{ z_language|default:"en"|escape }}" class="{% block html_class %} {% endblock %}">
    {% include "head/head.tpl" id=id %}

	{% with m.rsc[id.content_group_id] as content_group %}
		<body class="{% if content_group %}has-contentgroup {% endif %}{{ id.category.name }} {% block body_class %}{% endblock %} do_foundation do_base">
            {% block after_body %}{% endblock %}
    		{% block global_nav %}
    			{% catinclude "global-nav/global-nav.tpl" id %}
    		{% endblock %}

    		{% block content_group_nav %}
    			{% include "content-group-nav/content-group-nav.tpl" %}
    		{% endblock %}

            {% block content_area %}
    	       {% block content %}{% endblock %}
            {% endblock %}

    		{% block footer %}
    			{% include "footer/footer.tpl" %}
    		{% endblock %}

    		{% all include "_js_include.tpl" %}

            {% all include "_html_body.tpl" %}

            {% javascript %}
                $(document).trigger('widgetmanager:loaded');
            {% endjavascript %}

    		{% script %}

            {% block extra_scripts %}{% endblock %}
	   </body>
    {% endwith %}
</html>
