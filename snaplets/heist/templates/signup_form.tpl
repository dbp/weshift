<div-async name="signup-form">
  <ifGuest>
    <strong>If you already have an account on WeShift, be sure to log in above first.</strong><br/><br/>
  </ifGuest>
  <form-async action="/signup" method="POST">
    Organization:<br> 
    <div class="errors"><organization-errors><error/><br></organization-errors></div>
    <input type="text" name="organization" value="$(organization-value)"/><br>
    Place Name:<br>
    <div class="errors"><place-errors><error/><br></place-errors></div>
     <input type="text" name="place" value="$(place-value)" />
    <ifGuest>
      <br>Your Name:<br> 
      <div class="errors"><name-errors><error/><br></name-errors></div>
      <input type="text" name="name" value="$(name-value)" />
    </ifGuest>
    <button type="submit"/>
  </form-async>
</div-async>
