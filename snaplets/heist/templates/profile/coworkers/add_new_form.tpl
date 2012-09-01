<div-async name="add-coworker">
	We use email to contact people about covering shifts. The address can be removed anytime, and we won't ever give it out. Enter coworker's email below:
  <form-async method="POST" action="$(placeRoot)/coworkers/add/new">
  <input type="hidden" name="name" value="$(name)"/>
  <table>
    <tr><td colspan="2"><email-errors><error/><br></email-errors></td></tr>
    <tr><td>Email:</td>
      <td><input name="email" type="text" value="$(email-value)" /></td>
        <td><button type="submit" title=""/></td></tr>
    <tr><td colspan="2">Don't have / prefer not to</td><td><button type="submit" title=""/></td></tr>
  </table>
  </form-async>
</div-async>