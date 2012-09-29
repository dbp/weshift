<day>
  <div-async name="day-${dayNum}-loader" id="day-${dayNum}-loader"></div-async>
  <div-async name="day-${dayNum}" class="large">
    <div class="user-wrap ${selfClasses}">
      <userName/> <button class="add toggle" data-toggle-target=".add-form-${dayNum}">Add Shift</button>
        <apply template="shift/add">
          <bind tag="disp">none</bind>
        </apply>
      <selfShifts>
        <notDeadlineDone>
        <div class="shift ${color}">
          <div class="time">
            <description/>
            <isDeadline>by</isDeadline>
            <start/><notDeadline>-<stop/></notDeadline>
            (<units/>)
            <isDeadline>
              <button class="done toggle" data-toggle-target=".done-form-${id}">Done</button>
            </isDeadline>
          </div> 
          <apply template="shift/deadline_done">
            <bind tag="disp">none</bind>
          </apply>
          <apply template="shift/buttons"></apply>
          <apply template="shift/claims">
            <bind tag="claimee">true</bind>
          </apply>
        </div>
      </notDeadlineDone>
      </selfShifts>
    </div>
  
    <div class="${dayClasses}">
      <a-async href="${placeRoot}/month/${currYear}/${currMonth}/${dayNum}" class="close2-p" data-toggle-target=".add-form-${dayNum}"><dayNum/></a-async>
      <a-async href="${placeRoot}/day/${currYear}/${currMonth}/${dayNum}" data-loading-div="#center .main" class="expand" title="Day View"></a-async>
    </div>

    <div class="clearfix"/>

    <div class="other-wrap">
      <otherShifts>
        <notDeadlineDone>
        <div class="other-shift ${color}">
        <ifRequested>
          <button class="cover toggle" data-toggle-target=".cover-form-${id}"/>               
        </ifRequested>
        <user-lookup id="${user}"><name/></user-lookup> - <description/> <isDeadline>by</isDeadline> <start/><notDeadline>-<stop/></notDeadline> (<units/>)<br>
        <isFacilitator>
          <apply template="shift/buttons"></apply>
        </isFacilitator>
        <div class="buttons">
          <button class="claim toggle" data-toggle-target=".claim-form-${id}">Claim Part</button>
        </div>
        <apply template="shift/claim">
          <bind tag="disp">none</bind>
        </apply>
        </div>
        <apply template="shift/claims"></apply>
        </notDeadlineDone>
      </otherShifts>      
    </div>
  </div-async>
</day>

<closeDays>
  <div-async name="day-${dayNum}" class="holder"></div-async>
</closeDays>
