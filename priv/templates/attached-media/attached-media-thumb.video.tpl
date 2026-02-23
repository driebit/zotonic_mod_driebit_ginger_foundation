{% if
    id.medium.mime == "text/html-oembed" or
    id.medium.mime == "text/html-video-embed" %}

    {% if id.medium.oembed.provider_name|lower == "youtube" %}

        <a href="https://www.youtube.com/watch?v={{ id.medium.video_embed_id }}" class="lightbox lightbox-video-embed fancybox.iframe" rel="body" {% if id.summary %}title="{{ id.summary }}"{% endif %}>
            {% image id mediaclass="media-thumb" title=id.title alt=id.title %}
            <i class="fa fa-play-circle"></i>
        </a>

    {% elif id.medium.video_embed_service|lower == "youtube" %}

        <a href="https://www.youtube.com/watch?v={{ id.medium.video_embed_id }}" class="lightbox lightbox-video-embed fancybox.iframe" rel="body" {% if id.summary %}title="{{ id.summary }}"{% endif %}>
            {% image id mediaclass="media-thumb" title=id.title alt=id.title %}
            <i class="fa fa-play-circle"></i>
        </a>

    {% elif id.medium.oembed.provider_name|lower == "vimeo" %}

        <a href="https://player.vimeo.com/video/{{ id.medium.oembed.video_id }} " class="lightbox lightbox-video-embed fancybox.iframe" rel="body" {% if id.summary %}title="{{ id.summary }}"{% endif %}>
        {% image id mediaclass="media-thumb" title=id.title alt=id.title %}
            <i class="fa fa-play-circle"></i>
        </a>

    {% elif id.medium.video_embed_service|lower == "vimeo" %}
        <a href="https://vimeo.com/{{ id.medium.video_embed_id }}" class="lightbox lightbox-video-embed fancybox.iframe" rel="body" {% if id.summary %}title="{{ id.summary }}"{% endif %}>
            {% image id mediaclass="media-thumb" title=id.title alt=id.title %}
            <i class="fa fa-play-circle"></i>
        </a>

    {% endif %}

{% else %}

    <a href="#" data-video-url="{% url media_attachment id=id %}" data-video-width="{{ id.medium.width }}" data-video-height="{{ id.medium.height }}" class="lightbox lightbox-video-embed default-video-player" rel="body" {% if id.summary %}title="{{ id.summary }}"{% endif %}>
        {% image id mediaclass="media-thumb" title=id.title alt=id.title %}
    </a>
{% endif %}
