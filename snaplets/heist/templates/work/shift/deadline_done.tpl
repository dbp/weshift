<div-async name="done-form-${id}" class="done-form-${id} done-form" style="display: ${disp};">
  <form-async action="${placeRoot}/shift/deadline_done" method="POST">
    <input type="hidden" name="shift" value="${id}"/>
    Mark deadline done? <button type="submit"/>
  </form-async>
</div-async>