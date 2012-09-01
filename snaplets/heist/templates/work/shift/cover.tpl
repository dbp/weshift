<div-async name="cover-form-$(id)" class="cover-form-$(id) cover-form" style="display: $(disp);">
  <form-async action="$(placeRoot)/shift/cover" method="POST">
    <input type="hidden" name="user" value="$(user)"/>
    <input type="hidden" name="shift" value="$(id)"/>
    <input type="hidden" name="req" value="$(reqid)"/>
    Cover this shift? <button type="submit"/>
  </form-async>
</div-async>