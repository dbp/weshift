bean.add(document, 'DOMContentLoaded', function () {
  
  // generic close boxs (num is how far up to go)
  declare("click", ".close", true, function (elem) {
      bonzo(elem.parentNode).hide();
    });
  declare("click", ".close2", true, function (elem) {
      bonzo(elem.parentNode.parentNode).hide();
    });
    
  function shift(elem,ammount) {
    e = bonzo(elem);
    cur = e.css("left");
    n = Number(cur.substring(0,cur.length-2))
    e.css("left",(n + ammount) + "px");    
  }
  
  declare("click", ".shift-left", true, function (elem) {
      shift(qwery(elem.getAttribute("data-shift-target"))[0],1 - Number(elem.getAttribute("data-shift-ammount")));
    });
  declare("click", ".shift-right", true, function (elem) {
      shift(qwery(elem.getAttribute("data-shift-target"))[0],Number(elem.getAttribute("data-shift-ammount")));
    });
  
 
});