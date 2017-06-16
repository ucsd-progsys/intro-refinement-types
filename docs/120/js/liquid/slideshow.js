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

    //update window.location
    var url     = window.location.href;
    var nextUrl = setParameterByName(url, 'slide', nextSlide);
    history.pushState({}, "dummy", nextUrl);
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

//https://stackoverflow.com/questions/5999118/add-or-update-query-string-parameter
function setParameterByName(uri, key, value) {
  var re = new RegExp("([?&])" + key + "=.*?(&|$)", "i");
  var separator = uri.indexOf('?') !== -1 ? "&" : "?";
  if (uri.match(re)) {
    return uri.replace(re, '$1' + key + "=" + value + '$2');
  }
  else {
    return uri + separator + key + "=" + value;
  }
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


/* FWD-BACK with keydown -- DEPRECATED as it messes with TEXT BUFFER

$(document).keydown(function(e) {
       var tag = e.target.tagName.toLowerCase();
       if (tag != 'input' && tag != 'textarea'){
         switch(e.which) {
           case 37: // left
              console.log('LEFT-arrow: prev slide');
              gotoSlide(prevSlide(currSlide));
              e.preventDefault(); // prevent the default action (scroll / move caret)
              break;

           case 39: // right
              console.log('LEFT-arrow: prev slide');
              gotoSlide(nextSlide(currSlide));
              e.preventDefault(); // prevent the default action (scroll / move caret)
              break;

           // default: return; // exit this handler for other keys
         }
         // e.preventDefault(); // prevent the default action (scroll / move caret)
       }
   });
  */

// FORWARD with DOUBLE CLICK.
/* DISABLED but is it the PROBLEM ?!!
$('#page').dblclick(function(e) {
    console.log('DBL-click: next slide');
    gotoSlide(nextSlide(currSlide));

    // e.preventDefault();

    // clear text selection...
    if (window.getSelection)
          window.getSelection().removeAllRanges();
      else if (document.selection)
          document.selection.empty();
  });
*/

// YIKES $('#page').not('.ace_text-input')
  // YIKES //.not('textarea')
  // YIKES //$(':not( .ace_text-input )')
    // YIKES .mousedown(function(event) {
      // YIKES switch (event.which) {
          // YIKES case 1:
              // YIKES console.log('LEFT-click: next slide');
              // YIKES gotoSlide(nextSlide(currSlide));
              // YIKES event.preventDefault();
              // YIKES break;
          // YIKES case 3:
              // YIKES console.log('RIGHT-click: prev slide');
              // YIKES gotoSlide(prevSlide(currSlide));
              // YIKES event.preventDefault();
              // YIKES break;
          // YIKES default:
              // YIKES break; // alert('You have a strange Mouse!');
      // YIKES }
  // YIKES });

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

}
