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

/*$(\"a[data-value='tab.survival']\").removeAttr(\"data-toggle\")
                $(\"a[data-value='tab.hrqol']\").removeAttr(\"data-toggle\")
                $(\"a[data-value='tab.toxicity']\").removeAttr(\"data-toggle\")
                $(\"ul.nav>li:gt(0)\").addClass(\"disabled\")*/