<apply template="heading">
  <bind tag="timesheet_sel">sel</bind>
</apply>

<div-async name="center-main" class="main" id="timesheet">
  <h2>Timesheet | Hours: <totalHours/>, Units: <totalUnits/></h2>
    <form-async action="${placeRoot}/timesheet" method="POST">
    <div id="timesheet">
      <input class="date" type="text" name="start" value="${timesheetStart}"> 
      to 
      <input class="date" type="text" name="stop" value="${timesheetStop}">
      <isFacilitator>
        for
        <span>
          <select name="user">
            <timesheetCoworkers>
              <option value="${userId}" ${selected}><userName/></option>
            </timesheetCoworkers>
          </select>
        </span>
      </isFacilitator>
      <isNormalUser>
        <span><input type="hidden" name="user" value="${userId}"></span>        
      </isNormalUser>
      <button type="submit" title="Get Timesheet" /></div></form-async>
    <table id="entries"><tbody><tr class="heading">
      <td id="hours"><h5>Hours (Units)</h5></td>
      <td id="shift"><h5>Original Shift</h5></td>
      <td id="changes"><h5>History</h5></td>
    </tr>
    
    <timesheet>
      <tr>
        <td class="entry-hours">
          <notDeadline><hoursWorked/></notDeadline>
          <isDeadline>
            <isDeadlineDone>&#10003;</isDeadlineDone>
            <notDeadlineDone>&#10007;</notDeadlineDone>
          </isDeadline>
          (<units/>)</td>
        <td class="entry-shift"><startTime/><notDeadline>-<endTime/></notDeadline><br><shiftDate/></td>
        <td class="entry-changes">
          <changes>
            <div class="${changeClasses}"><changeDescription/> by <changePerson/><br>(<changeTime/>, <changeDate/>)</div>
          </changes>
      </tr>
    </timesheet>

    <!-- <tr>
      <td class="entry-hours">7</td>
      <td class="entry-shift">10:30AM-5:30PM<br>May 10, 2011</td>
      <td class="entry-changes"><div class="change">To 5:00AM-11:00AM by<br>Daniel Patterson<br>(11:04PM, 5.30.2011)</div></td>
    </tr><tr>
      <td class="entry-hours">0</td>
      <td class="entry-shift">5:00AM-10:00AM<br>May 11, 2011</td>
      <td class="entry-changes"><div class="delete">Deleted by Daniel Patterson<br>(11:04PM, 5.30.2011)</div></td>
    </tr><tr>
      <td class="entry-hours">7</td>
      <td class="entry-shift">6:45AM-1:45PM<br>May 13, 2011</td>
      <td class="entry-changes"></td>
    </tr> -->
    
    </tbody></table>
    
</div-async>