netAssess.tour = {slides: [], slideCount: 0, width: 400, height: 300, active: true}

netAssess.tour.makeSlide = function(options) {
  options = $.extend({
    position: "center",
    title: "NetAssess2020",
    text: "Welcome to NetAssess2020!",
    target: "#map",
    runbefore: function() {},
    runafter: function() {}
  }, options)
  netAssess.tour.slides.push(options)
}

netAssess.tour.makeSlide({
  text: "This tool is meant to assist with 5-year Network Assessments as required by 40 CFR &#0167;58.10(d).<br><br>You can start with this quick tour, or close this box with the 'x' in the top right corner and try it on your own. You can also refer to the <a href = 'https://github.com/USEPA/NetAssess2020' target='_blank'>NetAssess2020 Documentation Site</a> for more detailed information about the App."
})
                             
netAssess.tour.makeSlide({title: "Menu Bar",
  text: "Above is the menu bar. The menu bar is how you will access most of the functionality of the tool. Each icon above is a button that gives you access to a tool or function in the NetAssess2020 app.",
  target: "#menuControls",
  position: "below"
})
                                                  
netAssess.tour.makeSlide({title: "Layers",
  text: "This is the Layers control. Use the Layers control to switch between different base maps and to turn different tool layers on and off.<br><br>Right now, the grey base map is displayed. There is also a streets base map and a satellite base map. The grey base map is generally best for viewing output from the various tools in the NetAssess 2020 app.",
  target: ".leaflet-control-layers-toggle",
  position: "right"
})

netAssess.tour.makeSlide({title: "More Options",
  text: "These buttons open side panels that offer additional settings and information. The top button opens the 'Settings' sidebar, the middle button opens the 'Help' sidebar, and the bottom button opens the 'About' sidebar.",
  target: ".leaflet-top.leaflet-right",
  position: "left"
})

netAssess.tour.makeSlide({title: "Legend",
  text: "This is the legend. The legend explains what the symbols on the map mean. Pay attention to the legend because it will change depending on which tools your are using, and which layers you have displayed.",
  target: "#legendFloater",
  position: "above"
})

netAssess.tour.makeSlide({title: "Using NetAssess",
  text: "<p>Now we will cover how to get started using the tools.</p><p>The following steps do not require you to make any selections. The app will make appropriate selections to illustrate the functionality.</p>"
})

netAssess.tour.makeSlide({title: "Pollutant of Interest",
  text: "The first step in the assessment process should be selecting a pollutant. You can do that with the pollutant selection menu above.<br><br>Once you select a pollutant the map will update to show the locations of known sites monitoring for that pollutant. We have selected <b>O3 - Ozone (44201)</b> for this example.", 
  runbefore: function() {
    if($("#pollutantSelect").select2("val") != "ozone") {
       $("#pollutantSelect").select2("val", "ozone");
       $("#pollutantSelect").trigger("change");
    }
  }
})
                             
netAssess.tour.makeSlide({title: "Sites",
  text: "Once a pollutant is selected, all sites that monitor that pollutant will be displayed as red circles on the map.<br><br>You can click on a site to open a window that contains basic information about that site and a graph depicting the air quality trends for that site."
})
                             
netAssess.tour.makeSlide({title: "Area of Interest",
  text: "Next, you will need to select an Area of Interest. The Area of Interest focuses your analysis on a specific area of the country. You can open the Area of Interest window by clicking the button above.", 
  target: "#aoiButton", 
  position: "below",
  runafter: netAssess.floaters.aoi.open
})
                          
netAssess.tour.makeSlide({title: "Area of Interest",
  text: "From the Area of Interest dialog you can select an area of interest in several ways. You can draw an area free-hand, with the 'Draw an Area of Interest' controls.<br><br>Or, you can also select a predefined area such as a State, CBSA, or CSA. We have selected the State of Arkansas for this example.", 
  target: "#aoiFloater", 
  position: "right",
  runbefore: function() {
    $("[name='areaSelect'][value='State']").trigger("click");
    setTimeout(function() {
      $("#areaSelectSelect").select2("val", "05");
      $("#areaSelectSelect").trigger("change");
    }, 500)
  },
  runafter: netAssess.floaters.aoi.close
})

netAssess.tour.makeSlide({title: "Site Selection",
  text: "Notice the sites within the Area of Interest have gotten brighter and larger to indicate they are selected. You can also right-click on sites to open a menu that allows you to select, deselect, or hide those sites individually."
})

netAssess.tour.makeSlide({title: "New Sites",
  text: "At this point, you may want to add new sites to your monitoring network. Click this button, then click the location on the map where you want the new monitor to be placed.<br><br>New sites are depicted as green circles and are treated just like existing sites for the purposes of Area Served calculations.",
  target: "#newSiteButton",
  position: "below"
})

netAssess.tour.makeSlide({title: "Area Served",
  text: "The Area Served tool provides information regarding the area served by the monitors in your Area of Interest.<br><br>Clicking the button will calculate the Area Served by each monitor and draw polygons on the map representing those areas. You can then click on a polygon to get more information about that area.",
  target: "#areaServedButton",
  position: "below",
  runafter: function() { $("#areaServedButton").trigger("click"); }
})

netAssess.tour.makeSlide({title: "Area Served",
  text: "In a moment, grey polygons will appear around the selected sites. Each grey polygon represents the area that is closer to the monitor within it than any other monitor in the network.<br><br>Clicking on a polygon will open a new window with information about that area.",
  position: "right",
  runafter: function() {
    var key = Object.keys(netAssess.layerGroups.areaServed._layers)[1];
      netAssess.layerGroups.areaServed._layers[key].fire("click");
  }
})

netAssess.tour.makeSlide({title: "Area Served",
  text: "The Area Served Information window will appear to the right. The Area Served Information window gives you geographic and demographic information about the area you clicked.<br><br>If you have selected Ozone or PM<sub>2.5</sub> you will also be given the probability of a NAAQS violation within the Area of Interest.", 
  target: "#areaServedFloater",
  position: "left",
  runafter: netAssess.floaters.areaServed.close
})

netAssess.tour.makeSlide({title: "Correlation Matrix",
  text: "The Correlation Matrix tool gives you information about how concentrations at monitors within your Area of Interest compare to one another.<br><br>After clicking 'Next', a new window displaying the Correlation Matrix will open momentarily.", 
  target: "#cormatButton", 
  position: "below",
  runafter: function() { $("#cormatButton").trigger("click"); }
})

netAssess.tour.makeSlide({title: "Correlation Matrix",
  text: "Each monitor comparison is represented by a square. The blue squares in the bottom-left show the correlation between each pair of monitors and the number of days used to calculate the correlation.</p><p>The squares in the top-right show the mean absolute difference and the distance in kilometers between the two monitors. The numbers on the diagonal are the most recent design value for each monitor.",
  position: "right",
  runafter: netAssess.floaters.cormat.close
});
                          
netAssess.tour.makeSlide({title: "Exceedance Probabilities",
  text: "Exceedance Probabilities have been calculated for ozone and PM<sub>2.5</sub>. This information is available for each census tract in the continental United States.<br><br>You can view this information on the map by turning on the 'Ozone Probability' or 'PM<sub>2.5</sub> Probability' layers from the Layers Control. The PM<sub>2.5</sub> probability layer is currently displayed.",
  target: ".leaflet-control-layers-toggle",
  position: "right",
  runbefore: function() {netAssess.map.addLayer( netAssess.overlays.pm25) },
  runafter: function() { netAssess.map.removeLayer(netAssess.overlays.pm25) }
})
                          
netAssess.tour.makeSlide({title: "Removal Bias",
  text: "The Removal Bias tool finds the nearest neighbors to each selected monitor and then uses the concentrations at that those sites to interpolate the concentration at the monitoring site. It then compares that interpolation to the actual concentrations measured at the site.<br><br>If there is little difference (low bias) that may indicate that the monitor is redundant and could be removed.",
  target: "#rembiasButton",
  position: "below",
  runbefore: function() { $("#rembiasButton").trigger("click"); }
})

netAssess.tour.makeSlide({title: "Removal Bias",
  text: "Once the tool has run, the sites with data available will become larger and colored differently. The color represents the mean Removal Bias calculated for that site.<br><br>Refer to the legend for interpretation of the colors. Clicking on a site will open a window that gives more information about the Removal Bias at that location."
})
                          
netAssess.tour.makeSlide({title: "Zoom to Area of Interest",
  text: "This button can be used to zoom in to your area of interest, once you have defined one.",
  target: "#aoiZoomButton",
  position: "below"
})
                          
netAssess.tour.makeSlide({title: "Navigation",
  text: "This button will zoom you out to view the entire continental United States.",
  target: "#fullExtentButton",
  position: "below"
})
                          
netAssess.tour.makeSlide({title: "Download Data",
  text: "All data calculated by the NetAssess2020 app is available for download as CSV files. Clicking this button will open a dialog where you can choose the data you want to download.",
  target: "#downloadDataButton",
  position: "below",
  runafter: netAssess.floaters.download.open
})

netAssess.tour.makeSlide({title: "Download Data",
  text: "Click the download icon next to a data type to download that data. If the data type is greyed out that mean that you haven't made sufficient selections to generate that data yet.",
  target: "#downloadFloater",
  position: "below",
  runafter: netAssess.floaters.download.close
})

netAssess.tour.makeSlide({title: "Reset App",
  text: "This button will reset the app to its beginning state. All calculated layers will be erased all selections will be removed, and all settings will be restored to their default.",
  target: "#resetAppButton",
  position: "below"
})

netAssess.tour.makeSlide({title: "Conclusion",
  text: "This concludes the tour. You can check the 'Don't show again' box below to prevent this tour from opening automatically the next time you visit the app. You can always reopen it from the 'Help' sidebar to the right.",
  runbefore: function() {netAssess.resetApp()}
})

$(document).ready(function() {
  var name = "showtour"
  var showTour = "true";
  var ca = document.cookie.split(';');
  for(var i=0; i < ca.length; i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1);
    if (c.indexOf(name) != -1) showTour = c.substring(name.length+1,c.length);
  }
  if(showTour == "true") {
    netAssess.tour.active = true;
    document.getElementById("tourGone").checked = false;
  } else {
    netAssess.tour.active = false;
    document.getElementById("tourGone").checked = true;
  }
  if(netAssess.tour.active) {
    netAssess.tour.advance()
  } 
})

netAssess.tour.setPosition = function(target, position) {
  var rect = $(target)[0].getBoundingClientRect();
  var rect_center = {x: (rect.width / 2) + rect.left,
                     y: (rect.height / 2) + rect.top
  }
  var arrowPos = {
    "left": "",
    "top": "",
    "display": "none",
    "border-left-color": "transparent",
    "border-top-color": "transparent",
    "border-right-color": "transparent",
    "border-bottom-color": "transparent"
  }
  switch(position) {
    case "center":
      var position = {
        top: rect_center.y - (netAssess.tour.height / 2),
        left: rect_center.x - (netAssess.tour.width / 2),
        display: "block"
      }
      arrowPos.top = "";
      arrowPos.left = "";
      arrowPos.display = "none";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      break;
    case "above":
      var position = {
        top: rect.top - (netAssess.tour.height + 15),
        left: rect_center.x - (netAssess.tour.width / 2),
        display: "block"
      };
      arrowPos.top = netAssess.tour.height - 5;
      arrowPos.left = netAssess.tour.width / 2;
      arrowPos.display = "block";
      arrowPos["border-top-color"] = "#EFEFEF";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      arrowBorderPos.top = arrowBorderPos.top + 2.5
      arrowBorderPos["border-top-color"] = "black";
      break;
    case "below":
      var position = {
        top: rect.bottom + 15,
        left: rect_center.x - (netAssess.tour.width / 2),
        display: "block"
      };
      arrowPos.top = -20;
      arrowPos.left = (netAssess.tour.width / 2) - 10;
      arrowPos.display = "block";
      arrowPos["border-bottom-color"] = "#EFEFEF";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      arrowBorderPos.top = arrowBorderPos.top - 2.5
      arrowBorderPos["border-bottom-color"] = "black";
      break;
    case "left":
      var position = {
        top: rect_center.y - (netAssess.tour.height / 2),
        left: rect.left - (netAssess.tour.width + 15),
        display: "block"
      }
      arrowPos.top = (netAssess.tour.height / 2) - 10;
      arrowPos.left = netAssess.tour.width - 5;
      arrowPos.display = "block";
      arrowPos["border-left-color"] = "#EFEFEF";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      arrowBorderPos.left = arrowBorderPos.left + 2.5;
      arrowBorderPos["border-left-color"] = "black";
      break;
    case "right":
      var position = {
        top: rect_center.y - (netAssess.tour.height / 2),
        left: rect.right + 15,
        display: "block"
      }
      arrowPos.top = (netAssess.tour.height / 2) - 10;
      arrowPos.left = -20;
      arrowPos.display = "block";
      arrowPos["border-right-color"] = "#EFEFEF";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      arrowBorderPos.left = arrowBorderPos.left - 2.5;
      arrowBorderPos["border-right-color"] = "black";
      break;
    default:
      console.log("Unrecognized 'position' to setPosition function.")
  }
  var w = window.innerWidth;
  var h = window.innerHeight;
  if(position.left < 0) {
    var offset_x = 5 + (position.left + netAssess.tour.width);
  } else if((position.left + netAssess.tour.width) > w) {
    var offset_x = 5 + ((position.left + netAssess.tour.width) - w);
  } else {
    var offset_x = 0;
  }
  position.left = parseInt(position.left - offset_x, 10) + "px";
  arrowPos.left = parseInt(arrowPos.left + offset_x, 10) + "px";
  arrowBorderPos.left = parseInt(arrowBorderPos.left + offset_x, 10) + "px";
  if(position.top < 0) {
    var offset_y = 5 - position.top; 
  } else if((position.top + netAssess.tour.height) > h) {
    var offset_y = (position.top + netAssess.tour.height) - h;
  } else {
    var offset_y = 0;
  }
  position.top = parseInt(position.top + offset_y, 10) + "px";
  arrowPos.top = parseInt(arrowPos.top - offset_y, 10) + "px";
  arrowBorderPos.top = parseInt(arrowBorderPos.top - offset_y, 10) + "px";
  var $tour = $("#tour");
  $tour.css(position);
  $tour.find(".tour-arrow").css(arrowPos);
  $tour.find(".tour-arrow-border").css(arrowBorderPos);
}

netAssess.tour.advance = function() {
  var tour = netAssess.tour;
  var $tour = $("#tour");
  var cnt = tour.slideCount;
  if(cnt > 0) tour.slides[cnt - 1].runafter();
  tour.slides[cnt].runbefore();
  $tour.find(".header").html(tour.slides[cnt].title);
  $tour.find(".content")[0].scrollTop = 0;
  $tour.find(".content").html(tour.slides[cnt].text);
  tour.setPosition(tour.slides[cnt].target, tour.slides[cnt].position);
  tour.slideCount++
}

netAssess.tour.close = function() {
  netAssess.tour.active = false;
  $("#tour").css("display", "none")
  $("*").off(".tour");
}

$("#tourNext").on("click", function() {
  if(netAssess.tour.slideCount == netAssess.tour.slides.length - 1) {
    $("#tourNext").text("Close")
  } else {
    $("#tourNext").text("Next")
  }
  if(netAssess.tour.slideCount == netAssess.tour.slides.length) {
    netAssess.tour.close()
  } else {
    netAssess.tour.advance()
  }
})

$("#tour .close").on("click", netAssess.tour.close);

$("#tour #tourGone").on("click", function(e) {
  var d = new Date();
  d.setTime(d.getTime() + (60*24*60*60*1000));
  var expires = "expires="+d.toUTCString();
  if(this.checked) {
    document.cookie = "showtour=false; " + expires
  } else {
    document.cookie = "showtour=true; " + expires
  }
})

$("#openTour").on("click", function() {
  if(netAssess.tour.active == false) {
    netAssess.tour.slideCount = 0;
    netAssess.tour.active = true;
    netAssess.tour.advance();
    netAssess.sidebars.help.hide();
  }
})

// Disabled the Next button until the page has a chance to load
$("#tourNext").attr("disabled", true);
$(document).ready(function() {
  setTimeout(function() {$("#tourNext").attr("disabled", false)}, 1500)
})