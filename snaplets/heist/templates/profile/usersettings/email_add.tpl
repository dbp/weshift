<div-async name="add-email">
  <dfForm data-async="1" action="${placeRoot}/settings/email/add" method="POST">
  <table>
    <tr><td colspan="2"><dfErrorList ref="address"/></td></tr>
    <tr><td class="label"><dfLabel ref="address">Add:</dfLabel></td> 
      <td><dfInputText ref="address" />
        <button type="submit" title=""/></td></tr>
  </table>
  </dfForm>
</div-async>