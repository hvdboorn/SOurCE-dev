$(document).ready(function(){
    $('[data-toggle="tooltip"]').tooltip();
    $('[data-toggle="popover"]').popover();
});

/*Disable clicking if lp() is still NULL*/
$("ul.nav > li").on("click", function(e) {
  if ($(this).hasClass("disabled")) {
    e.preventDefault();
    return false;
  }
});