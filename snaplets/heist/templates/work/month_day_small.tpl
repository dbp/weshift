<day>
  <div id="day-${dayNum}" class="${boxClasses}">
    <div class="${dayClasses}">
      <a-async href="${placeRoot}/month/${mCurrYear}/${mCurrMonth}/${dayNum}/large" data-loading-div="#day-${dayNum}-loader"><dayNum/></a-async>
    </div>
    
    <large>
      <apply template="month_day_large"/>
    </large>
    <notLarge>
      <div-async name="day-${dayNum}-loader" id="day-${dayNum}-loader"></div-async>
      <div-async name="day-${dayNum}" class="holder">
      </div-async>
    </notLarge>
  </div>
</day>