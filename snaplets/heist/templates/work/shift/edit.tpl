<div-async name="change-form-${id}" class="change-form-${id} change-form" style="display: ${disp};">
  	<dfForm data-async="1" action="${placeRoot}/shift/edit" method="POST">
            <dfInputHidden ref="user" data-default="${user}"/>
    		<dfInputHidden ref="id" data-default="${id}"/>
    		<dfInputHidden ref="day" data-default="${dayNum}"/>
    		<dfInputHidden ref="month" data-default="${currMonth}"/>
    		<dfInputHidden ref="year" data-default="${currYear}"/>
			<dfInputText ref="start" data-default="${start}"/> to <dfInputText ref="stop" data-default="${stop}"/>
			<dfLabel ref="units">units</dfLabel>
            <dfInputText ref="units" data-default="${units}"/>
            <wsSelect name="color" data-default="${color}">
                <option value="Transparent">None</option>
                <option value="Blue">Blue</option>
                <option value="Red">Red</option>
                <option value="Green">Green</option>
            </wsSelect>
            <dfChildErrorList class="errors" />
            
			<button type="submit"/>
  	</dfForm>
</div-async>