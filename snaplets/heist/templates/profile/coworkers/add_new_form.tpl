<div-async name="add-coworker">
	We use email to contact people about covering shifts. The address can be removed anytime, and we won't ever give it out. Enter coworker's email below:
  <dfForm data-async="1" method="POST" action="${placeRoot}/coworkers/add/new">
  <input type="hidden" name="name" value="${name}"/>
  <table>
    <tr><td colspan="2"><dfErrorList ref="email"/></td></tr>
    <tr><td><dfLabel ref="email">Email:</dfLabel></td>
      <td><dfInputText ref="email"/></td>
        <td><button type="submit" title=""/></td></tr>
    <tr><td colspan="2">Don't have / prefer not to</td><td><button type="submit" title=""/></td></tr>
  </table>
  </dfForm>
</div-async>