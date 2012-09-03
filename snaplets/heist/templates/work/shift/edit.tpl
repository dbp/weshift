<div-async name="change-form-${id}" class="change-form-${id} change-form" style="display: ${disp};">
  	<dfForm data-async="1" action="${placeRoot}/shift/edit" method="POST">
    		<dfInputHidden ref="id" data-default="${id}"/>
    		<dfInputHidden ref="day" data-default="${dayNum}"/>
    		<dfInputHidden ref="month" data-default="${currMonth}"/>
    		<dfInputHidden ref="year" data-default="${currYear}"/>
			<dfInputText ref="start" data-default="${start}"/> to <dfInputText ref="stop" data-default="${stop}"/>
			<dfChildErrorList class="errors" />
			<button type="submit"/>
  	</dfForm>
</div-async>