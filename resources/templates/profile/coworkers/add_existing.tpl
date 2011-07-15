<div-async name="add-coworker">
  We found people named <name/> already on WeShift. Did you mean any of these people?
  <br><br>
  <users>
    <form-async method="POST" action="$(placeRoot)/coworkers/add/exists">
      <input type="hidden" name="id" value="$(id)"/>
      <name/> at <places><name/>, <org/> - </places>
      <button type="submit"/>
    </form-async>
  </users>
  <br><br>
  If this is a new person, optionally enter an email address:<br>
  <apply template="add_new_form"/>
</div-async>