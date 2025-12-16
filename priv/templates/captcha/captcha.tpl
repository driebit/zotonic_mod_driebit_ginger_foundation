{# If a recaptcha key is known, then include the script dynamically #}
{% if m.driebit_ginger_foundation.recaptcha_key as key %}
    {% javascript %}
        if (!document.getElementById('recaptcha-script') && typeof grecaptcha === 'undefined') {
            const s = document.createElement("script");
            const h = document.getElementsByTagName('head')[0];

            s.type = "text/javascript";
            s.id = 'recaptcha-script';
            s.nonce = z_script_nonce;
            s.src = "https://www.google.com/recaptcha/api.js?render={{ key }}";
            h.appendChild(s);
        }

        window.captchaValidate = function( value, args, isSubmit, submitTrigger ) {
            grecaptcha.ready(function() {
                grecaptcha.execute('{{ key }}', { action: 'submit' })
                    .then(function(token) {
                        // Add your logic to submit to your backend server here.
                        document.getElementById("{{ #captcha }}").value = token;
                        z_async_validation_result('{{ #captcha }}', true, token);
                    });
            });
            return 'async';
        }
    {% endjavascript %}

    <div class="form-group" style="dispay: none">
        <input type="hidden" id="{{ #captcha }}" name="recaptcha-token" value="">
        {% validate id=#captcha
                    name="recaptcha-token"
                    type={custom against="window.captchaValidate" isAsync}
        %}
    </div>

    <div id="captcha-failed" style="display: none">
        <p class="alert alert-error">{_ Sorry, there was a problem with processing your form. Please contact us if the problem persists._}</p>
    </div>

{% endif %}
