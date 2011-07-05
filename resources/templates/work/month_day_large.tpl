<day>
  <div-async name="day-$(dayNum)" id="day-$(dayNum)" class="$(boxClasses) large">
    <div class="user-wrap">
      <userName/> - 
      <a-async href="$(placeRoot)/month/$(currYear)/$(currMonth)/$(dayNum)/small">X</a-async>
    </div>
  
    <div class="$(dayClasses)">
      <a-async href="$(placeRoot)/month/$(currYear)/$(currMonth)/$(dayNum)/small"><dayNum/></a-async>
    </div>

    This is where all the stuff will go!
  </div-async>
</day>
