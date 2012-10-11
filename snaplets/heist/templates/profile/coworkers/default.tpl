<apply template="base">

<coworkers>
  <div class="${classes}">
    <name/>
    <notfac>
    	<isFacilitator>
      <button class="facilitate-button toggle" data-toggle-target=".facilitate-form-${id}"/>
      <button class="delete-button toggle" data-toggle-target=".delete-form-${id}"/>
      <inActive>
        <button class="activate-button toggle" data-toggle-target=".activate-link-${id}"/>
      </inActive>
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
      <div class="activate-link-${id}" style="display: none">
        Visit <a href="/activate/account?id=${id}&token=${activationLink}&pl=${placeId}">Activation Link</a>
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