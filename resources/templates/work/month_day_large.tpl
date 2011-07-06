<day>
  <div-async name="day-$(dayNum)" id="day-$(dayNum)" class="$(boxClasses) large">
    <div class="user-wrap">
      <userName/><br>
      <selfShifts>
        <start/>-<stop/> - Request Off - Delete Shift - Change Shift
      </selfShifts>
      <a-async href="$(placeRoot)/month/$(currYear)/$(currMonth)/$(dayNum)/small">X</a-async>
    </div>
  
    <div class="$(dayClasses)">
      <a-async href="$(placeRoot)/month/$(currYear)/$(currMonth)/$(dayNum)/small"><dayNum/></a-async>
    </div>

    <otherShifts>
      <user lookup="$(user)"><name/></user> - <start/>-<stop/>
    </otherShifts>
  </div-async>
</day>
