$('#password').keypress(function (e) {
  var key = e.which;
  if(key == 13)  // the enter key code
  {
    $('#login_button').click();
    return false;  
  }
});