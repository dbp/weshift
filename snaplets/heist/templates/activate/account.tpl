<apply template="base">

  <bind tag="center">
  <div class="content">
    <h3>Please set your password:</h3>
    <dfForm method="POST">
      <table>
        <tr><td colspan="2"><dfErrorList ref="new"/></td></tr>
        <tr><td class="label">
          <dfLabel ref="new">Password:</dfLabel></td> 
          <td><dfInputPassword ref="new" /></td></tr>
        <tr><td colspan="2"><dfErrorList ref="confirm"/></td></tr>
        <tr><td class="label">
          <dfLabel ref="confirm">Again:</dfLabel></td> 
          <td><dfInputPassword ref="confirm" />
          <button type="submit" title=""/></td></tr>
      </table>
    </dfForm>
  </div> <!-- .content -->
  
  </bind>
  

</apply>