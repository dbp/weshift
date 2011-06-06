<apply template="base">
  <apply template="heading">
    <bind tag="name_sel">sel</bind>
  </apply>

  <div class="content">
    <h3>Change Name</h3>
    <div-async name="name-change-form">
    <form-async action="$(placeRoot)/settings/name" method="POST">
      <table>
        <tr><td colspan="2"><name-errors><error/><br></name-errors></td></tr>
        <tr><td class="label"><label for="name">Name:</label></td> 
          <td><input name="name" type="text" />
          <button type="submit" title=""/></td></tr>
      </table>
    </form-async>
    </div-async>
  </div> <!-- .content -->

</apply>