<div-async name="request-form-${id}" class="request-form-${id} request-form" style="display: ${disp};">
  <form-async action="${placeRoot}/shift/requestoff" method="POST">
    <input type="hidden" name="shift" value="${id}"/>
    <label for="notify">Email Coworkers?</label>
    <input type="checkbox" checked="checked" name="notify"/><br/>
    Ask someone to cover? <button type="submit"/>
  </form-async>
</div-async>