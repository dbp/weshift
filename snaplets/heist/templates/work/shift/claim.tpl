<div-async name="claim-form-${id}" class="claim-form-${id} claim-form" style="display: ${disp};">
    <dfForm data-async="1" action="${placeRoot}/shift/claim" method="POST">
            <dfInputHidden ref="id" data-default="${id}"/>
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
            <dfLabel ref="units">units</dfLabel>
            <dfInputText ref="units" data-default="0"/>
            <dfLabel ref="reason">reason</dfLabel>
            <dfInputText ref="reason" data-default=""/>
            <dfChildErrorList class="errors" />
            <br/>
            Claim part of this shift? <button type="submit"/>
    </dfForm>
</div-async>