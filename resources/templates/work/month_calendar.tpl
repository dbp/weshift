<apply template="heading">
  <bind tag="month_sel">sel</bind>
</apply>

<div-async name="center-main" class="main" id="month">
  <h2>Monthly Calendar</h2>
  <div id="month-heading">
    <h1><a-async href="$(placeRoot)/month/$(prevYear)/$(prevMonth)" id="prev"></a-async> 
        <monthName/> 
        <a-async href="$(placeRoot)/month/$(nextYear)/$(nextMonth)" id="next"></a-async></h1>
  </div> <!-- #month-heading -->
  <div id="month-daynames">
    <div class="dayname">Sun</div>
    <div class="dayname">Mon</div>
    <div class="dayname">Tue</div>
    <div class="dayname">Wed</div>
    <div class="dayname">Thu</div>
    <div class="dayname">Fri</div>
    <div class="dayname">Sat</div>
  </div> <!-- #month-daynames -->
  <div id="month-days">
    <monthDays>
      <div class="$(boxClasses)">
        <div class="$(dayClasses)" data-href="$(placeRoot)/month/$(currYear)/$(currMonth)/$(dayNum)">
          <dayNum/>
        </div>
      </div>
    </monthDays>
  </div> <!-- #month-days -->

</div-async> <!-- .main -->

<script type="text/javascript" charset="utf-8">
  $('#month-days .day').not('.none').each(function (i) {
    $(this).bt({
    fill: '#F7F7F7', 
    
    ajaxPath: ["$(this).attr('data-href')"],
    ajaxLoading:     '<strong>Loading...</strong>',
    // ajaxCache:        false,
    // ajaxType:         'POST',  
    // 
    // contentSelector: "$(this).children('.day-info').html()",
    // preShow: function (box) {
    //   var change = $(box).find(".change");
    //   change.hide();
    //   change.find(".cancel").click(function () {
    //     change.hide();
    //     $(box).find(".shift").show();
    //   });
    //   $(box).find(".show-change").click(function () {
    //     $(this).parent().hide();
    //     change.show();
    //   });
    //   },
    strokeStyle: '#B7B7B7', 
    spikeLength: 10, 
    spikeGirth: 10, 
    padding: 8, 
    cornerRadius: 0,
    trigger: 'click',
    closeWhenOthersOpen: true,
    positions: ['bottom'],
    cssStyles: {
      fontFamily: '"lucida grande",tahoma,verdana,arial,sans-serif', 
      fontSize: '11px',
      width: '420px'
    }
  });});
</script>