<div-async name="change-form-${id}" class="change-form-${id} change-form" style="display: ${disp};">
  	<show notblank="${id}">
  		<form-async action="${placeRoot}/shift/edit" method="POST">
    		<input type="hidden" name="id" value="${id}"/>
    		<input type="hidden" name="day" value="${dayNum}"/>
    		<input type="hidden" name="month" value="${currMonth}"/>
    		<input type="hidden" name="year" value="${currYear}"/>
    		<input type="text" name="start"/> to <input type="text" name="stop" />
    		<button type="submit"/>
    	</form-async>
    </show>
    <show blank="${id}">    
  		<dfForm data-async="1" action="${placeRoot}/shift/edit" method="POST">
    		<dfInputHidden ref="id"/>
    		<dfInputHidden ref="day"/>
    		<dfInputHidden ref="month"/>
    		<dfInputHidden ref="year"/>
			<dfInputText ref="start"/> to <dfInputText ref="stop"/>
			<dfChildErrors />
			<button type="submit"/>
  		</dfForm>
	</show>
</div-async>