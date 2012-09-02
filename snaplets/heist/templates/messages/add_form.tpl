<div-async name="add-message">
  <dfForm data-async="1" action="${placeRoot}/messages/add" method="POST">
  	<dfErrorList ref="message"/>
    <dfInputTextArea ref="message"></dfInputTextArea>
    <button type="submit" title="Post Message"></button>
  </dfForm>
</div-async>