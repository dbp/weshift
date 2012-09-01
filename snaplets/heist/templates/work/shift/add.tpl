<div-async name="add-form-$(dayNum)$(day-value)" class="add-form-$(dayNum)$(day-value) add-form" style="display: $(disp);">
  <form-async action="$(placeRoot)/shift/add" method="POST">
    <isFacilitator>
        <select name="user">
            <option value="$(userId)" selected="selected"><userName/></option>
            <allUsers>
                <not-current>
                    <option value="$(id)"><name/></option>
                </not-current>
            </allUsers>
        </select>
    </isFacilitator>
    <isNormalUser>
        <input type="hidden" name="user" value="$(userId)"/>
    </isNormalUser>
    <input type="hidden" name="day" value="$(dayNum)$(day-value)"/>
    <input type="hidden" name="month" value="$(currMonth)$(month-value)"/>
    <input type="hidden" name="year" value="$(currYear)$(year-value)"/>
    <input type="text" name="start" value="$(start-value)"/> to <input type="text" name="stop" value="$(stop-value)"/><button type="submit"/>
    <div class="errors"><start-errors><error/><br></start-errors></div>
  </form-async>
</div-async>