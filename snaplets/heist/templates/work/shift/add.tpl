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
    <dfInputHidden ref="day" value="${dayNum}${day-value}"/>
    <dfInputHidden ref="month" value="${currMonth}${month-value}"/>
    <dfInputHidden ref="year" value="${currYear}${year-value}"/>
    <dfInputText ref="start"/> to <dfInputText ref="stop"/><button type="submit"/>
    <dfErrorList ref="start"/>
  </dfForm>
</div-async>