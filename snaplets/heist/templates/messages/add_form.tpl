<div-async name="add-message">
  <form-async action="$(placeRoot)/messages/add" method="POST">
    <div class="errors"><message-errors><error/><br></message-errors></div>
    <textarea name="message"><show notblank="$(message-value)"><message-value/></show></textarea>
    <button type="submit" title="Post Message"></button>
  </form-async>
</div-async>