<h3>Change Name</h3>
<div-async name="name-change-form">
	<show blank="${name}">
		<form-async action="${placeRoot}/settings/name" method="POST">
  		  <table>
  		    <tr><td colspan="2"></td></tr>
  		    <tr><td class="label"><label ref="name">Name:</label></td> 
  		      <td><input type="text" name="change-name-form.name" value="${userName}"/>
  		      <button type="submit" title=""/></td></tr>
  		  </table>
  		</form-async>
	</show>
	<show notblank="${name}">
  		<dfForm data-async="1" action="${placeRoot}/settings/name" method="POST">
  		  <table>
  		    <tr><td colspan="2"><dfErrorList ref="name"/></td></tr>
  		    <tr><td class="label"><dfLabel ref="name">Name:</dfLabel></td> 
  		      <td><dfInputText ref="name"/>
  		      <button type="submit" title=""/></td></tr>
  		  </table>
  		</dfForm>
  	</show>
</div-async>
