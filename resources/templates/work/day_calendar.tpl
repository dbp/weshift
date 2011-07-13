<apply template="heading">
  <bind tag="day_sel">sel</bind>
</apply>

<div-async name="center-main" class="main" id="day">
  <h2>Daily Calendar</h2>
  <div id="day-heading">
    <h1><a-async href="$(placeRoot)/day/$(dPrevYear)/$(dPrevMonth)/$(dPrevDay)" id="prev"></a-async> 
        <dayName/> 
        <a-async href="$(placeRoot)/day/$(dNextYear)/$(dNextMonth)/$(dNextDay)" id="next"></a-async></h1>
  </div> <!-- #month-heading -->

  <div id="day-calendar">
    <div id="time"><timeColumn/></div>
    <div class="shift-right" id="shift-right" data-shift-ammount="60" data-shift-target="#day-calendar .view .holder" data-shift-width="421"/>
    <div class="shift-left" id="shift-left" data-shift-ammount="61" data-shift-target="#day-calendar .view .holder" data-shift-width="421"/>   
    <div class="view h-$(viewHeight)">
      <div class="holder h-$(viewHeight)">
      <div id="day-workernames" style="width: $(workersWidth)px;">
        <dayWorkers>
          <div class="worker"><name/></div>
        </dayWorkers>
      </div> <!-- #month-daynames -->
      <div class="columns">
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
      </div>
    </div>
  </div>
  
</div-async>