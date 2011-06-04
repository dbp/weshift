<apply template="base">
  <apply template="settings_heading">
    <bind tag="password_sel">sel</bind>
  </apply>

  <div class="content">
    <h3>Change Password</h3>
    <form>
      <table>
        <tr><td class="label"><label for="current">Current:</label></td> <td><input name="current" type="password" /></td></tr>
        <tr><td class="label"><label for="password">New:</label></td> <td><input name="password" type="password" /></td></tr>
        <tr><td class="label"><label for="confirm">Confirm:</label></td> <td><input name="confirm" type="password" /><button type="submit" title=""/></td></tr>
      </table>
    </form>
  </div> <!-- .content -->

</apply>