<apply template="base">

<coworkers>
  <div class="$(classes)">
    <name/>
    <notfac>
    	<isFacilitator>
      <button class="facilitate-button toggle" data-toggle-target=".facilitate-form-$(id)"/>
      <div class="facilitate-form-$(id)" style="display: none">
    		<form-async action="$(placeRoot)/coworkers/facilitate" class="facilitate-form">
    		<input type="hidden" name="id" value="$(id)"/>
        Make this user a facilitator?
    		<button type="submit" value=""/>
    		</form-async>
      </div>
    	</isFacilitator>
    </notfac>
  </div>
</coworkers>

<isFacilitator>
  <h4>Add New Coworker</h4>
  <apply template="add"/>
</isFacilitator>

</apply>