<div-async name="messages" id="messages">
  <messages>
  <div class="message" id="message-$(id)">
    <p><message/></p>
    <div class="info">
      <div class="timestamp"> <timestamp/><!-- written 12:01AM, 4.10.2011 --></div>
      <div class="message-forms">
        <div class="up"><a-async href="$(placeRoot)/messages/vote/up/$(id)" title="Vote up">+<ups/></a-async></div>
        <div class="down"><a-async href="$(placeRoot)/messages/vote/down/$(id)" title="Vote down">-<downs/></a-async></div>
        <div class="flag"><a-async href="$(placeRoot)/messages/flag/$(id)" title="Flag as inappropriate">&nbsp;</a-async></div>
      </div>
    </div>
  </div>
  </messages>
   <div id="messages-nav">
    <pages>
      <a-async data-loading-div="#messages" href="$(placeRoot)/messages/page/$(num)" class="$(class)"><num/></a-async>
    </pages>
    <show notblank="$(next)">
      <a-async data-loading-div="#messages" href="$(placeRoot)/messages/page/$(next)" id="next"></a-async>
    </show>
  </div>
</div-async>