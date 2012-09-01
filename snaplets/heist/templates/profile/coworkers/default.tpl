<apply template="base">

<coworkers>
  <div class="${classes}">
    <name/>
    <notfac>
    	<isFacilitator>
      <button class="facilitate-button toggle" data-toggle-target=".facilitate-form-${id}"/>
      <button class="delete-button toggle" data-toggle-target=".delete-form-${id}"/>
      <div class="facilitate-form-${id}" style="display: none">
    		<form-async action="${placeRoot}/coworkers/facilitate" class="facilitate-form">
    		<input type="hidden" name="id" value="${id}"/>
        Make this user a facilitator?
    		<button type="submit" value=""/>
    		</form-async>
      </div>
      <div class="delete-form-${id}" style="display: none">
    		<form-async action="${placeRoot}/coworkers/delete" class="delete-form">
    		<input type="hidden" name="id" value="${id}"/>
        Are you sure you want to remove this user?
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