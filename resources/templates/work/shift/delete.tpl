<div-async name="delete-form-$(id)" class="delete-form-$(id) delete-form" style="display: $(disp);">
  <form-async action="$(placeRoot)/shift/delete" method="POST">
    <input type="hidden" name="shift" value="$(id)"/>
    Really delete this shift? <button type="submit"/>
  </form-async>
</div-async>