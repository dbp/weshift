<div-async name="add-form-${dayNum}" class="add-form-${dayNum} add-form" style="display: ${disp};">
  <dfForm data-async="1" action="${placeRoot}/shift/add" method="POST">
    <isFacilitator>
        <wsSelect name="user" data-default="${userId}">
            <allUsers>
                <option value="${id}"><name/></option>
            </allUsers>
        </wsSelect>
    </isFacilitator>
    <isNormalUser>
        <dfInputHidden ref="user" data-default="${userId}"/>
    </isNormalUser>
    <dfInputHidden ref="day" data-default="${dayNum}"/>
    <dfInputHidden ref="month" data-default="${currMonth}"/>
    <dfInputHidden ref="year" data-default="${currYear}"/>

    <label for="ws.deadline">Deadline</label>
    <input type="checkbox" name="ws.deadline" class="deadline-toggle"/>

    <dfInputText ref="start" data-default="9:00am"/> to <dfInputText ref="stop" data-default="5:00pm"/>
    <dfLabel ref="units">units</dfLabel>
    <dfInputText ref="units" data-default="0"/>
    <dfLabel ref="color">color</dfLabel>
    <wsSelect name="color" data-default="None">
        <option value="Transparent">None</option>
        <option value="Blue">Blue</option>
        <option value="Red">Red</option>
        <option value="Green">Green</option>
    </wsSelect>
    <dfChildErrorList class="errors"/>
    <button type="submit"/>
  </dfForm>
</div-async>