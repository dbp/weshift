<apply template="heading">
  <bind tag="timesheet_sel">sel</bind>
</apply>

<div-async name="center-main" class="main" id="timesheet">
  <h2>Timesheet | Total Hours: 14.</h2>
    <form><div id="timesheet"><input type="hidden" name="place" value="1">
      <input type="text" name="start" value="2011-5-1"> 
      to 
      <input type="text" name="stop" value="2011-5-30">
      for
      <span><input type="hidden" name="user" value="1"><input disabled="disabled" value="Daniel Patterson" /></span>
      <button type="submit" title="Get Timesheet" /></div></form>
    <table id="entries"><tbody><tr class="heading">
      <td id="hours"><h5>Hours Worked</h5></td>
      <td id="shift"><h5>Original Shift</h5></td>
      <td id="changes"><h5>History</h5></td>
    </tr><tr>
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
    </tr></tbody></table>
</div-async>