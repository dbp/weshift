<h3>Change Name</h3>
<div-async name="name-change-form">
  <dfForm data-async="1" action="${placeRoot}/settings/name" method="POST">
    <table>
      <tr><td colspan="2"><dfErrorList ref="name" class="errors"/></td></tr>
      <tr><td class="label"><dfLabel ref="name">Name:</dfLabel></td> 
        <td><dfInputText ref="name"/>
        <button type="submit" title=""/></td></tr>
    </table>
  </dfForm>
</div-async>
