<day>
  <div-async name="day-$(dayNum)" id="day-$(dayNum)" class="$(boxClasses)">
    <div class="$(dayClasses)">
      <a-async href="$(placeRoot)/month/$(currYear)/$(currMonth)/$(dayNum)/large" data-loading-div="#day-$(dayNum) .loading-holder"><dayNum/></a-async>
    </div>
    <div class="loading-holder"><br><br></div>
  </div-async>
</day>