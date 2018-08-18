$(document).on("shiny:visualchange", function(e) {
	Shiny.onInputChange("innerHeight", $("#surv_plot_options").height());
	Shiny.onInputChange("innerWidthLijn",$("#out.surv__km").width());
  Shiny.onInputChange("innerWidthPicto",$("#out.surv__pictograph").width());
});

$(window).resize(function(e) {
	Shiny.onInputChange("innerHeight", $("#surv_plot_options").height());
	Shiny.onInputChange("innerWidthLijn",$("#out.surv__km").width());
	Shiny.onInputChange("innerWidthPicto",$("#out.surv__pictograph").width());
});