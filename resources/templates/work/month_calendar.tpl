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
    <monthDays></monthDays>
  </div> <!-- #month-days -->

</div-async> <!-- .main -->