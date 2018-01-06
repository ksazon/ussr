function showSelectService(service){
  $('#select-service-message').css("display","block");
  var active_list = $('.active-select-service');
  active_list.removeClass("active-select-service").addClass("disactive-select-service");
  $("#" + service).removeClass("disactive-select-service").addClass("active-select-service");
  $("#" + service).prop('selectedIndex',0);

}

function getcalendar(service_code){
  console.log("Odpalam " + service_code);

  $.ajax({
      url : "/company/get_calendar/", // the endpoint
      type : "POST", // http method
      data : {'service_code' : service_code}, // data sent with the post request

      // handle a successful response
      success : function(response) {
          $('#calendar').remove();
          $('#calendar-box').html(response);
          console.log("success"); // another sanity check
      },

  });

}

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

  $('#select-service-group').change(function(){
    console.log(this.text);
    showSelectService(this.value);

  });

});

window.onload = function() {
  var services_lists = document.getElementsByClassName("select-service");
  for (var i = 0; i < services_lists.length; i++) {
      services_lists[i].addEventListener("change", function(){
          getcalendar(this.value); })
        };
};
