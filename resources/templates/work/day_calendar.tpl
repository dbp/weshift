<apply template="heading">
  <bind tag="day_sel">sel</bind>
</apply>

<div-async name="center-main" class="main" id="day">
  <h2>Daily Calendar</h2>
  <div id="day-heading">
    <h1><a-async href="$(placeRoot)/day/$(prevYear)/$(prevMonth)/$(prevDay)" id="prev"></a-async> 
        <dayName/> 
        <a-async href="$(placeRoot)/day/$(nextYear)/$(nextMonth)/$(nextDay)" id="next"></a-async></h1>
  </div> <!-- #month-heading -->
  <div id="day-workernames">
    <dayWorkers>
      <div class="worker"><name/></div>
    </dayWorkers>
  </div> <!-- #month-daynames -->

  <div id="day-calendar">
    <div id="time"><timeColumn/></div>
    
    <dayWorkers>
      <div class="column h-$(columnHeight)">
        <shifts>
          <div class="shift o-$(offset) h-$(length) $(classes)">
            <div class="time">
              <time/>
            </div>
          </div>
        </shifts>
      </div>
    </dayWorkers>
  </div>
  
</div-async>