<apply template="base">
  <apply template="usersettings_heading">
    <bind tag="remove_sel">sel</bind>
  </apply>

  <div class="content">
    <h3>Disable Account</h3>
    <show notblank="${msg}">
      <msg/><br><br>
    </show>
    NOTE: You must have a confirmed email to do this. It will be deleted in the process.<br><br>
    <dfForm data-async="1" action="${placeRoot}/settings/remove" method="POST">
        Remove this account <button type="submit" title=""/>
    </dfForm>
  </div> <!-- .content -->

</apply>