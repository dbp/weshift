<div-async name="add-coworker">
  <form-async method="POST" action="$(placeRoot)/coworkers/add/new">
  <input type="hidden" name="name" value="$(name)"/>
  <table>
    <tr><td colspan="2"><email-errors><error/><br></email-errors></td></tr>
    <tr><td>Add email (optional):</td>
      <td><input name="email" type="text" value="$(email-value)" />
        <button type="submit" title=""/></td></tr>
  </table>
  </form-async>
</div-async>