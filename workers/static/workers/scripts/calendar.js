function get_worker_calendar() {
    $.ajax({
        url : "/services/get_worker_calendar/", // the endpoint
        type : "GET", // http method

          success : function(response) {
            $('#worker-calendar').append(response);
        },
    });
};


$(document).ready(function() {

  function csrfSafeMethod(method) {
      // these HTTP methods do not require CSRF protection
      return (/^(GET|HEAD|OPTIONS|TRACE)$/.test(method));
  }

  $.ajaxSetup({
      beforeSend: function(xhr, settings) {
          if (!csrfSafeMethod(settings.type) && !this.crossDomain) {
              xhr.setRequestHeader("X-CSRFToken", csrftoken);
          }
      }
  });

  get_worker_calendar();
});
