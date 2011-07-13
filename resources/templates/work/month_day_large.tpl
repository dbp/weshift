<day>
  <div-async name="day-$(dayNum)-loader" id="day-$(dayNum)-loader"></div-async>
  <div-async name="day-$(dayNum)" class="large">
    <div class="user-wrap $(selfClasses)">
      <userName/> <button class="add toggle" data-toggle-target=".add-form-$(dayNum)">Add Shift</button>
        <apply template="shift/add">
          <bind tag="disp">none</bind>
        </apply>
      <selfShifts>
        <div class="shift">
          <div class="time">
            <start/>-<stop/>
          </div> 
          <div class="buttons">
            <button class="request toggle" data-toggle-target=".request-form-$(dayNum)">Request Off</button> 
            <button class="delete toggle" data-toggle-target=".delete-form-$(dayNum)">Delete Shift</button>
          </div>
          <div class="request-form-$(dayNum) request-form" style="display: none;">
            <form-async target="$(placeRoot)/shift/request" method="POST">
              <input type="hidden" name="shift" value="$(id)"/>
              Ask someone to cover? <button type="submit"/>
            </form-async>
          </div>
          <div class="delete-form-$(dayNum) delete-form" style="display: none;">
            <form-async target="$(placeRoot)/shift/delete" method="POST">
              <input type="hidden" name="shift" value="$(id)"/>
              Really delete this shift? <button type="submit"/>
            </form-async>
          </div>
          <div class="buttons">
            <button class="change toggle" data-toggle-target=".change-form-$(dayNum)">Change Shift</button>
          </div>
          <div class="change-form-$(dayNum) change-form" style="display: none;">
            <form-async target="$(placeRoot)/shift/change" method="POST">
              <input type="hidden" name="shift" value="$(id)"/>
              <input type="text" name="start" value="$(start)"/> to <input type="text" name="stop" value="$(stop)"/><button type="submit"/>
            </form-async>
          </div>
        </div>
      </selfShifts>
    </div>
  
    <div class="$(dayClasses)">
      <button class="close2"><dayNum/></button>
      <a-async href="$(placeRoot)/day/$(currYear)/$(currMonth)/$(dayNum)" data-loading-div="#center .main" class="expand" title="Day View"></a-async>
    </div>

    <div class="clearfix"/>

    <div class="other-wrap">
      <otherShifts>
        <user-lookup id="$(user)"><name/></user-lookup> - <start/>-<stop/><br>
      </otherShifts>

      
    </div>
  </div-async>
</day>

<closeDays>
  <div-async name="day-$(dayNum)" class="holder"></div-async>
</closeDays>