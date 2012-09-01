<div-async name="add-email">
  <form-async action="${placeRoot}/settings/email/add" method="POST">
  <table>
    <tr><td colspan="2"><address-errors><error/><br></address-errors></td></tr>
    <tr><td class="label"><label for="address">Add:</label></td> 
      <td><input name="address" type="text" value="${address}" />
        <button type="submit" title=""/></td></tr>
  </table>
  </form-async>
</div-async>