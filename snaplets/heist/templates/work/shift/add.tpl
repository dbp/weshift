<div-async name="add-form-${dayNum}${day-value}" class="add-form-${dayNum}${day-value} add-form" style="display: ${disp};">
  <dfForm data-async="1" action="${placeRoot}/shift/add" method="POST">
    <isFacilitator>
        <select name="user">
            <option value="${userId}" selected="selected"><userName/></option>
            <allUsers>
                <not-current>
                    <option value="${id}"><name/></option>
                </not-current>
            </allUsers>
        </select>
    </isFacilitator>
    <isNormalUser>
        <dfInputHidden ref="user"/>
    </isNormalUser>
    <dfInputHidden ref="day" data-default="${dayNum}"/>
    <dfInputHidden ref="month" data-default="${currMonth}"/>
    <dfInputHidden ref="year" data-default="${currYear}"/>
    <dfInputText ref="start" data-default="9:00am"/> to <dfInputText ref="stop" data-default="5:00pm"/>
    <dfErrorList ref="start"/>
    <dfLabel ref="units">units</dfLabel>
    <dfInputText ref="units" data-default="0"/>
    <dfLabel ref="color">color</dfLabel>
    <wsSelect name="color" data-default="None">
        <option value="None">None</option>
        <option value="Blue">Blue</option>
        <option value="Red">Red</option>
        <option value="Green">Green</option>
    </wsSelect>
    <button type="submit"/>
  </dfForm>
</div-async>