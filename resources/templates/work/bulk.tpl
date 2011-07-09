<apply template="heading">
  <bind tag="bulk_sel">sel</bind>
</apply>

<div-async name="center-main" class="main" id="bulk">
   <h2>Bulk Input</h2>
   <show notblank="$(status)">
      <status/><br>
    </show>
   <form-async action="$(placeRoot)/bulk/upload" method="POST">
     <textarea name="data"><show notblank="$(data)"><data/></show></textarea>
     <button type="submit" title="Load Data"></button>
   </form-async>
   <p><strong>Important:</strong> What you paste in should look like the following. It is a format called CSV, which most spreadsheet programs can save as / export to.</p>
   <code>Name,5/2/2011,5/3/2011,5/4/2011,5/5/2011,5/6/2011<br>
   Jane Doe,7-3pm,7-3pm,off,7-3pm,7-3pm<br>
   Fishy Jones,11-7pm,10-4,11-7pm,11-7pm,11-7pm</code>
   <p>WeShift will attempt to read all the shifts, and any that it cannot read it will ignore, which means you can put anything you like (or nothing at all) in days that are off.</p>  
   <p>You will be able to see the shifts that it could read and confirm before they are actually added.</p>        
 </div-async>