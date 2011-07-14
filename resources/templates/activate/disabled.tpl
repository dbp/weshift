<apply template="base">

  <bind tag="center">
  <div class="content">
    <h3>Please set your password:</h3>
    <form method="POST">
      <table>
        <tr><td colspan="2"><new-errors><error/><br></new-errors></td></tr>
        <tr><td class="label">
          <label for="new">Password:</label></td> 
          <td><input name="new" type="password" /></td></tr>
        <tr><td colspan="2"><confirm-errors><error/><br></confirm-errors></td></tr>
        <tr><td class="label">
          <label for="confirm">Again:</label></td> 
          <td><input name="confirm" type="password" />
          <button type="submit" title=""/></td></tr>
      </table>
    </form>
  </div> <!-- .content -->
  
  </bind>
  

</apply>