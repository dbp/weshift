bean.add(document, 'DOMContentLoaded', function () {
  
  // generic close boxs (num is how far up to go)
  declare("click", ".close", true, function (elem) {
      bonzo(elem.parentNode).hide();
    });
  declare("click", ".close2", true, function (elem) {
      bonzo(elem.parentNode.parentNode).hide();
    });
    
  // version that doesnt prevent propogation
  declare("click", ".close2-p", false, function (elem) {
      bonzo(elem.parentNode.parentNode).hide();
    });
    
  function shift(elem,ammount,width) {
    e = bonzo(elem);
    cur = e.css("left");
    n = Number(cur.substring(0,cur.length-2));
    if (n + ammount > 0 ) {
      return; // would be leaving a gap on the left side
    } else if (ammount + n + elem.offsetWidth < width) {
         return; // would be leaving a gap on the right side
    } else {
      e.css("left",(n + ammount) + "px");      
    }

  }
  
  declare("click", ".shift-left", true, function (elem) {
      shift(qwery(elem.getAttribute("data-shift-target"))[0],1 - Number(elem.getAttribute("data-shift-ammount")), elem.getAttribute("data-shift-width"));
    });
  declare("click", ".shift-right", true, function (elem) {
    shift(qwery(elem.getAttribute("data-shift-target"))[0], Number(elem.getAttribute("data-shift-ammount")), elem.getAttribute("data-shift-width"));
    });
    
  declare("click",".toggle", true, function (elem){
    m = bonzo(qwery(elem.getAttribute("data-toggle-target"))[0]);
    if (m.css("display") === "none") {
      m.css("display", "block");
      bonzo(elem).addClass("toggled");
    } else {
      m.css("display", "none");
      bonzo(elem).removeClass("toggled");
    }
  });
  
 
});