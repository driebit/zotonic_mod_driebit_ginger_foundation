<div id="{{ list_id }}-buttons" class="search__pager">
    <div class="search__pager__result-counter">
        {% if items.is_total_estimated %}{{ _"About {n} results"|replace:"{n}":(items.total|round_significant|to_binary) }}
        {% else %}{{ items.total }} {_ results _}
        {% endif %}
    </div>
</div>
