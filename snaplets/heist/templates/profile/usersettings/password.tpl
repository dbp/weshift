<apply template="base">
  <apply template="usersettings_heading">
    <bind tag="password_sel">sel</bind>
  </apply>

  <div class="content">
    <h3>Change Password</h3>
    <div-async name="password-change-form">
    <dfForm data-async="1" action="${placeRoot}/settings/password" method="POST">
      <table>
        <tr><td colspan="2"><dfErrorList ref="current" class="errors"/></td></tr>
        <tr><td class="label"><dfLabel ref="current">Current:</dfLabel></td> <td><dfInputPassword ref="current" /></td></tr>
        <tr><td colspan="2"><dfErrorList ref="new" class="errors"/></td></tr>
        <tr><td class="label"><dfLabel ref="new">New:</dfLabel></td> <td><dfInputPassword ref="new" /></td></tr>
        <tr><td colspan="2"><dfErrorList ref="confirm" class="errors"/></td></tr>
        <tr><td class="label"><dfLabel ref="confirm">Confirm:</dfLabel></td> <td><dfInputPassword ref="confirm" />
          <button type="submit" title=""/></td></tr>
      </table>
    </dfForm>
    </div-async>
  </div> <!-- .content -->

</apply>