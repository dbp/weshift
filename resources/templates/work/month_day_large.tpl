<day>
  <div-async name="day-$(dayNum)-loader" id="day-$(dayNum)-loader"></div-async>
  <div-async name="day-$(dayNum)" class="large">
    <div class="user-wrap">
      <userName/><br>
      <selfShifts>
        <start/>-<stop/> <!-- - Request Off - Delete Shift - Change Shift -->
      </selfShifts>
    </div>
  
    <div class="$(dayClasses)">
      <button class="close2"><dayNum/></button>
    </div>

    <div class="other-wrap">
      <otherShifts>
        <user-lookup id="$(user)"><name/></user-lookup> - <start/>-<stop/>
      </otherShifts>
    </div>
  </div-async>
</day>
