<div-async name="change-form-${id}${id-value}" class="change-form-${id}${id-value} change-form" style="display: ${disp};">
  <form-async action="${placeRoot}/shift/edit" method="POST">
    <input type="hidden" name="id" value="${id}${id-value}"/>
    <input type="text" name="start" value="${start-value}"/> to <input type="text" name="stop" value="${stop-value}"/><button type="submit"/>
    <input type="hidden" name="day" value="${dayNum}${day-value}"/>
    <input type="hidden" name="month" value="${currMonth}${month-value}"/>
    <input type="hidden" name="year" value="${currYear}${year-value}"/>
    <div class="errors"><start-errors><error/><br></start-errors><id-errors><error/><br></id-errors></div>
  </form-async>
</div-async>