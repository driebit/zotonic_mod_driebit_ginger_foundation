<div id="{{ list_id }}-buttons" class="search__pager">
    <div class="search__pager__result-counter">
        {% if items.is_total_estimated %}
            {% trans "About {n} results" n=items.total|round_significant %}
        {% else %}
            {{ items.total }} {_ results _}
        {% endif %}
    </div>
</div>
