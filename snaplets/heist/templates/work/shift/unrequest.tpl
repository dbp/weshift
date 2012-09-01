<div-async name="unrequest-form-$(id)" class="unrequest-form-$(id) unrequest-form" style="display: $(disp);">
  <form-async action="$(placeRoot)/shift/unrequestoff" method="POST">
    <input type="hidden" name="shift" value="$(id)"/>
    <input type="hidden" name="reqid" value="$(reqid)"/>
    Cancel this request? <button type="submit"/>
  </form-async>
</div-async>