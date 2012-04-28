
    <div class="clearfix"/>

    <div class="other-wrap">
      <otherShifts>
        <ifRequested>
          <button class="cover toggle" data-toggle-target=".cover-form-$(id)"/>               
        </ifRequested>
        <user-lookup id="$(user)"><name/></user-lookup> - <start/>-<stop/><br>
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
  <div-async name="day-$(dayNum)" class="holder"></div-async>
</closeDays>