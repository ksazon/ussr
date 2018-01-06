function resignation(id_resources_usage){
  $.ajax({
      url : "/services/service_resignation/", // the endpoint
      type : "POST", // http method
      data : {'id_resources_usage' : id_resources_usage}, // data sent with the post request

      // handle a successful response
      success : function(response) {
        $('#resignation-message').html(response);
        $('#reservation-resignation-box').modal('hide');
        $('#confirmed-resignation-box').modal('show');
      },

  });
}


function OnResignationWindow(){
  $('#reservation-resignation-box').modal('show');
}

function OffResignationWindow(){
  $('#reservation-resignation-box').modal('hide');
}

function show_confirmation_window(chosen){

  OnResignationWindow();

  $('#confirm-button').click(function(e){
    e.preventDefault();
    console.log(chosen);
    resignation(chosen);
  });

  $('#decline-button').click(function(e){
    e.preventDefault();
    OffResignationWindow();
    console.log('Declined')
    $( "#confirm-button" ).unbind();
    $( "#decline-button" ).unbind();
  });
}

$(document).ready(function() {

  $('.resignation-button').click(function(e){
    e.preventDefault();
    var chosen = this.value;
    show_confirmation_window(chosen);
  });


});
