<apply template="base">
  <apply template="usersettings_heading">
    <bind tag="email_sel">sel</bind>
  </apply>

  <div class="content">
    <h3>Email Addresses</h3>
    <show notblank="$(msg)">
      <msg/>
    </show>
    <div-async name="email-settings" id="email-settings">
    <emails>
      <address/> <confirmed><span title="confirmed">&#9730;</span></confirmed> <a-async class="delete" href="$(placeRoot)/settings/email/delete/$(id)" data-loading-div="#email-settings">x</a-async> <br>
    </emails>
    <apply template="email_add"/>
    </div-async>
  </div> <!-- .content -->

</apply>