<apply template="heading">
  <bind tag="bulk_sel">sel</bind>
</apply>


<div-async name="center-main" class="main" id="bulk">
   <h2>Bulk Input:</h2>
   <form-async method="POST" action="$(placeRoot)/bulk/confirm">
      Understood:<br>
      <understood>
       <user-lookup id="$(user)"><name/></user-lookup> - <date/> - <start/>-<stop/><br>
      </understood>
      <input type="hidden" name="understood" value="$(understood-serialized)"/>
      <br>
      Not Understood:<br>
      <not-understood>
      <name/> - <date/> - <shift/><br>
      </not-understood>
      <input type="hidden" name="notunderstood" value="$(not-understood-serialized)"/>
      <button type="submit"/>
    </form-async>
</div-async>