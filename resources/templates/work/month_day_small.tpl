<day>
  <div id="day-$(dayNum)" class="$(boxClasses)">
    <div class="$(dayClasses)">
      <a-async href="$(placeRoot)/month/$(currYear)/$(currMonth)/$(dayNum)/large" data-loading-div="#day-$(dayNum) .holder"><dayNum/></a-async>
    </div>
    <div-async name="day-$(dayNum)" class="holder">
    </div-async>
  </div>
</day>