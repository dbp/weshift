<apply template="heading">
  <bind tag="bulk_sel">sel</bind>
</apply>

<div-async name="center-main" class="main" id="bulk">
   <h2>Bulk Input</h2>
   <show notblank="${status}">
      <status/><br>
    </show>
   <form-async action="${placeRoot}/bulk/upload" method="POST">
     <textarea name="data"><show notblank="${data}"><data/></show></textarea>
     <button type="submit" title="Load Data"></button>
   </form-async>
   <p><strong>Important:</strong> What you paste in should look like the following (the date format is particularly important). It is a format called CSV, which most spreadsheet programs can save as / export to.</p>
   <code>Name,10/2/2012,10/3/2012,10/4/2012<br>
   Jane Doe,7-3pm (1),7-3pm (1; register),off<br>
   Fishy Jones,11-7pm (3; R; opening),10-4 (R; 2.5),11-7pm</code>
   <p>The parenthesis and values inside of them are optional - they set the color (one character abreviations), the number of units for the shift, and a description. The colors letters are the first letter of the colors, which you can find when creating or editing shifts.</p>
   <p><strong>Also:</strong> Facilitators can load shifts for all workers. Non-facilitators: only your own shifts will be loaded.</p>
   <p>WeShift will attempt to read all the shifts, and any that it cannot read it will ignore, which means you can put anything you like (or nothing at all) in days that are off.</p>  
   <p>You will be able to see the shifts that it could read and confirm before they are actually added. If nothing shows up, it most likely means that your date format does not match (it must be MONTH/DAY/YEAR)</p>        
 </div-async>