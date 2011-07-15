<div-async name="add-coworker">
  <form-async method="POST" action="$(placeRoot)/coworkers/add">
  <table>
    <tr><td><name-errors><error/><br></name-errors></td></tr>
    <tr>
      <td><input name="name" type="text" value="$(name-value)" />
        <button type="submit" title=""/></td></tr>
  </table>
  </form-async>
</div-async>