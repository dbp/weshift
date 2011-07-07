<day>
  <div-async name="day-$(dayNum)" class="large">
    <div class="user-wrap">
      <userName/><br>
      <selfShifts>
        <start/>-<stop/> <!-- - Request Off - Delete Shift - Change Shift -->
      </selfShifts>
      <!-- <a-async href="$(placeRoot)/month/$(currYear)/$(currMonth)/$(dayNum)/small">X</a-async> -->
    </div>
  
    <div class="$(dayClasses)">
      <button class="close2"><dayNum/></button>
      <!-- <a-async href="$(placeRoot)/month/$(currYear)/$(currMonth)/$(dayNum)/small"></a-async> -->
    </div>

    <div class="other-wrap">
      <otherShifts>
        <user-lookup id="$(user)"><name/></user-lookup> - <start/>-<stop/>
      </otherShifts>
    </div>
  </div-async>
</day>
