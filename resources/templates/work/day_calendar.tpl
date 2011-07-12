<apply template="heading">
  <bind tag="day_sel">sel</bind>
</apply>

<div-async name="center-main" class="main" id="day">
  <h2>Daily Calendar</h2>
  <div id="month-heading">
    <h1><a-async href="$(placeRoot)/day/$(prevYear)/$(prevMonth)/$(prevDay)" id="prev"></a-async> 
        <dayName/> 
        <a-async href="$(placeRoot)/day/$(nextYear)/$(nextMonth)/$(nextDay)" id="next"></a-async></h1>
  </div> <!-- #month-heading -->
  <div id="month-daynames">
    <dayWorkers>
      <div class="dayname"><name/></div>
    </dayWorkers>
  </div> <!-- #month-daynames -->

  
  
</div-async>