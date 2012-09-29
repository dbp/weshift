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
  <apply template="unrequest">
    <bind tag="disp">none</bind>
  </apply>
</ifRequested>
<notRequested>
  <apply template="request">
    <bind tag="disp">none</bind>
  </apply>  
</notRequested>        
<apply template="delete">
  <bind tag="disp">none</bind>
</apply>
<div class="buttons">
  <button class="change toggle" data-toggle-target=".change-form-${id}">Change Shift</button>
  <notDeadline>
    <button class="split toggle" data-toggle-target=".split-form-${id}">Split Shift</button>
  </notDeadline>
</div>
<apply template="edit">
  <bind tag="disp">none</bind>
  <bind tag="start-value"><start/></bind>
  <bind tag="stop-value"><stop/></bind>
</apply>
<apply template="split">
  <bind tag="disp">none</bind>
  <bind tag="start-value"><start/></bind>
  <bind tag="stop-value"><stop/></bind>
</apply>