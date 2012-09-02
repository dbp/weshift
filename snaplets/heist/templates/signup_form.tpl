<div-async name="signup-form">
  <ifGuest>
    <strong>If you already have an account on WeShift, be sure to log in above first.</strong><br/><br/>
  </ifGuest>
  <dfForm data-async="1" action="/signup" method="POST">
    <dfLabel ref="organization">Organization:</dfLabel><br> 
    <dfErrorList ref="organization" class="errors"/>
    <dfInputText ref="organization" /><br>
    <dfLabel ref="place">Place Name:</dfLabel><br>
    <dfErrorList ref="place" class="errors"/>
    <dfInputText ref="place" />
    <ifGuest>
      <br>
      <dfLabel ref="name">Your Name:</dfLabel><br>
      <dfErrorList ref="name" class="errors"/>
      <dfInputText ref="name" />
    </ifGuest>
    <button type="submit"/>
  </dfForm>
</div-async>
