<day>
  <div-async name="day-${dayNum}-loader" id="day-${dayNum}-loader"></div-async>
  <div-async name="day-${dayNum}" class="large">
    <div class="user-wrap ${selfClasses}">
      <userName/> <button class="add toggle" data-toggle-target=".add-form-${dayNum}">Add Shift</button>
        <apply template="shift/add">
          <bind tag="disp">none</bind>
        </apply>
      <selfShifts>
        <div class="shift">
          <div class="time">
            <start/>-<stop/>
          </div> 
          <div class="buttons">
            <ifRequested>
              <button class="unrequest toggle" data-toggle-target=".unrequest-form-${id}">Stop Request</button>               
            </ifRequested>
            <notRequested>
              <button class="request toggle" data-toggle-target=".request-form-${id}">Request Off</button> 
            </notRequested>
            <button class="delete toggle" data-toggle-target=".delete-form-${id}">Delete Shift</button>
          </div>
          <ifRequested>
            <apply template="shift/unrequest">
              <bind tag="disp">none</bind>
            </apply>
          </ifRequested>
          <notRequested>
            <apply template="shift/request">
              <bind tag="disp">none</bind>
            </apply>  
          </notRequested>        
          <apply template="shift/delete">
            <bind tag="disp">none</bind>
          </apply>
          <div class="buttons">
            <button class="change toggle" data-toggle-target=".change-form-${id}">Change Shift</button>
          </div>
          <apply template="shift/edit">
            <bind tag="disp">none</bind>
            <bind tag="start-value"><start/></bind>
            <bind tag="stop-value"><stop/></bind>
          </apply>
        </div>
      </selfShifts>
    </div>
  
    <div class="${dayClasses}">
      <a-async href="${placeRoot}/month/${currYear}/${currMonth}/${dayNum}" class="close2-p" data-toggle-target=".add-form-${dayNum}"><dayNum/></a-async>
      <a-async href="${placeRoot}/day/${currYear}/${currMonth}/${dayNum}" data-loading-div="#center .main" class="expand" title="Day View"></a-async>
    </div>

    <div class="clearfix"/>

    <div class="other-wrap">
      <otherShifts>
        <ifRequested>
          <button class="cover toggle" data-toggle-target=".cover-form-${id}"/>               
        </ifRequested>
        <user-lookup id="${user}"><name/></user-lookup> - <start/>-<stop/><br>
        <isFacilitator>
          <button class="delete toggle" data-toggle-target=".delete-form-$(id)"></button> 
        </isFacilitator>
        <br/>
        <isFacilitator>
          <apply template="shift/delete">
            <bind tag="disp">none</bind>
          </apply>
        </isFacilitator>
        <ifRequested>
          <apply template="shift/cover">
            <bind tag="disp">none</bind>
          </apply>
        </ifRequested>
      </otherShifts>

      
    </div>
  </div-async>
</day>

<closeDays>
  <div-async name="day-${dayNum}" class="holder"></div-async>
</closeDays>
