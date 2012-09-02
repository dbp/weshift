<div-async name="add-coworker">
  <dfForm data-async="1" method="POST" action="${placeRoot}/coworkers/add">
  <table>
    <tr><td><dfErrorList ref="name"/></td></tr>
    <tr>
      <td><dfInputText ref="name"/>
        <button type="submit" title=""/></td></tr>
  </table>
  </dfForm>
</div-async>