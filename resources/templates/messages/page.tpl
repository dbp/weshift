<div-async name="messages" id="messages">
  <messages>
  <div class="message">
    <p><message/></p>
    <div class="info">
      <div class="timestamp"> <timestamp/><!-- written 12:01AM, 4.10.2011 --></div>
      <div class="message-forms">
        <div class="up"><a-async href="$(placeRoot)/messages/vote_up?message=$(id)" title="Vote up">+<ups/></a-async></div>
        <div class="down"><a-async href="$(placeRoot)/messages/vote_down?message=$(id)" title="Vote down">-<downs/></a-async></div>
        <div class="flag"><a-async href="$(placeRoot)/messages/flag?message=$(id)" title="Flag as inappropriate">&nbsp;</a-async></div>
      </div>
    </div>
  </div>
  </messages>
  <!-- <div class="message">
    <p>testtest2</p>
    <div class="info">
      <div class="timestamp"> written 4:19PM, 4.15.2011</div>
      <div class="message-forms">
        <div class="up"><a href="/vote_up?message=5">+1</a></div>
        <div class="down"><a href="http://www.weshift.org/vote_down?message=5">-0</a></div>
        <div class="flag"><a href="http://www.weshift.org/flag?message=5">&nbsp;</a></div>
      </div>
    </div>
  </div>
  <div class="message">
    <p>testes1
    </p>
    <div class="info">
      <div class="timestamp"> written 4:20PM, 4.15.2011</div>
      <div class="message-forms">
        <div class="up"><a href="/vote_up?message=6">+0</a></div>
        <div class="down"><a href="http://www.weshift.org/vote_down?message=6">-0</a></div>
        <div class="flag"><a href="http://www.weshift.org/flag?message=6">&nbsp;</a></div>
      </div>
    </div>
  </div>
  <div class="message">
    <p>hey everyone! maybe we could all log time that we do on this page.</p>
    <div class="info">
      <div class="timestamp"> written 9:41AM, 5.21.2011</div>
      <div class="message-forms">
        <div class="up"><a href="/vote_up?message=9">+0</a></div>
        <div class="down"><a href="http://www.weshift.org/vote_down?message=9">-0</a></div>
        <div class="flag"><a href="http://www.weshift.org/flag?message=9">&nbsp;</a></div>
      </div>
    </div>
  </div>
  <div class="message">
    <p>testest</p>
    <div class="info">
      <div class="timestamp"> written 4:19PM, 4.15.2011</div>
      <div class="message-forms">
        <div class="up"><a href="/vote_up?message=4">+0</a></div>
        <div class="down"><a href="http://www.weshift.org/vote_down?message=4">-0</a></div>
        <div class="flag"><a href="http://www.weshift.org/flag?message=4">&nbsp;</a></div>
      </div>
    </div>
  </div> -->
  <div id="messages-nav">
    <pages>
      <a-async href="$(placeRoot)/messages/page/$(num)" class="$(class)"><num/></a-async> <!-- class="sel" -->
    </pages>
    <show notblank="$(next)">
      <a-async href="$(placeRoot)/messages/page/$(next)" id="next"></a-async>
    </show>
  </div>
</div-async>