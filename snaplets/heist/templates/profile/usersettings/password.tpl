<apply template="base">
  <apply template="usersettings_heading">
    <bind tag="password_sel">sel</bind>
  </apply>

  <div class="content">
    <h3>Change Password</h3>
    <div-async name="password-change-form">
    <form-async action="${placeRoot}/settings/password" method="POST">
      <table>
        <tr><td colspan="2"><current-errors><error/><br></current-errors></td></tr>
        <tr><td class="label"><label for="current">Current:</label></td> <td><input name="current" type="password" /></td></tr>
        <tr><td colspan="2"><new-errors><error/><br></new-errors></td></tr>
        <tr><td class="label">
          <label for="new">New:</label></td> 
          <td><input name="new" type="password" /></td></tr>
        <tr><td colspan="2"><confirm-errors><error/><br></confirm-errors></td></tr>
        <tr><td class="label">
          <label for="confirm">Again:</label></td> 
          <td><input name="confirm" type="password" />
          <button type="submit" title=""/></td></tr>
      </table>
    </form-async>
    </div-async>
  </div> <!-- .content -->

</apply>