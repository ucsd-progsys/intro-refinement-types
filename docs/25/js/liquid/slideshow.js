/******************************************************************************/
/************** Setting Up SlideShow ******************************************/
/******************************************************************************/

var allSlides  = $('.slide');
var currSlide  = 0;
var firstSlide = 0;
var lastSlide  = allSlides.length - 1;

/* nextSlide :: (Int) => Int */
function nextSlide(cur){
  return (cur < allSlides.length - 1) ? cur + 1 : cur ;
}

/* prevSlide :: (Int) => Int */
function prevSlide(cur){
  return cur ? cur - 1 : cur;
}

/* gotoSlide :: (Int) => void */
function gotoSlide(nextSlide){
  if (nextSlide !== currSlide) {
    $(allSlides[currSlide])
          .removeClass('active')
          .addClass('inactive');
    $(allSlides[nextSlide])
          .removeClass('inactive')
          .addClass('active');

    currSlide = nextSlide;
  }
}

//Sigh. JS. https://stackoverflow.com/questions/901115/how-can-i-get-query-string-values-in-javascript
function getParameterByName(name) {
    var url = window.location.href;
    name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
}

$(function () {

  // Set slide num from URL on load. 
  var slideNum = parseInt(getParameterByName('slide'));

  if (slideNum >= 0){
    currSlide = slideNum;
  }

  // Initialize: Hide all
  $('.slide').removeClass('active').addClass('inactive');
  $(allSlides[currSlide]).removeClass('inactive').addClass('active');

  // Update
  $('.prevbutton').click(function (event) {
    console.log('prev slide click');
    gotoSlide(prevSlide(currSlide));
    event.preventDefault();
   });

  $('.nextbutton').click(function (event) {
    console.log('next slide click');
    gotoSlide(nextSlide(currSlide));
    event.preventDefault();
   });


  $('#page').on("swipeleft",function(event){
    console.log('next slide swipeleft');
    gotoSlide(nextSlide(currSlide));
    event.preventDefault();
  });

  $('#page').on("swiperight",function(event){
    console.log('next slide swiperight');
    gotoSlide(prevSlide(currSlide));
    event.preventDefault();
  });


  $('.firstbutton').click(function (event) {
    console.log('first slide click');
    gotoSlide(firstSlide);
    event.preventDefault();
   });

  $('.lastbutton').click(function (event) {
    console.log('last slide click');
    gotoSlide(lastSlide);
    event.preventDefault();
   });

});

/* progPaneSlide :: (Int) => Int */
function progPaneSlide(paneId){
  var paneId = "#program-pane-" + paneId;
  var elem   = $(paneId).closest(".slide");
  return allSlides.index(elem);

/*
var paneId = "#program-pane-1";
var elem   = $(paneId).closest(".slide");
var pos    = allSlides.index(elem);
 */
}
