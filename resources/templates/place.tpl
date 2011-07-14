<apply template="base">

<bind tag="left">
        <view is="profile">
          <apply template="profile/blank"></apply>
        </view>
        <view is="profile.settings">
          <apply template="profile/usersettings/blank"></apply>
        </view>
        <view is="profile.settings.name">
          <apply template="profile/usersettings/name"></apply>
        </view>
        <view is="profile.settings.password">
          <apply template="profile/usersettings/password"></apply>
        </view>
        <view is="profile.settings.email">
          <apply template="profile/usersettings/email"></apply>
        </view>
        <view is="profile.settings.remove">
          <apply template="profile/usersettings/remove"></apply>
        </view>
        <view is="profile.coworkers">
          <apply template="profile/coworkers/default"></apply>
        </view>
        <view is="profile.help">
          <apply template="profile/help/blank"></apply>
        </view>
</bind>

    <bind tag="center">
       <div class="heading">
         <div class="icon"><img src="/img/main/WorkSchedule.png" /></div>
         <div class="content">
           <h1>Work Schedule</h1>
           <h5>Next Shift: <nextShift/></h5>
         </div>
       </div> <!-- .heading -->
       
       <view has="work.month">
         <apply template="work/month_calendar"/>
       </view>
       <view has="work.day">
         <apply template="work/day_calendar"/>
       </view>
       <view is="work.timesheet">
         <apply template="work/timesheet"/>
       </view>
       <view is="work.bulk">
         <apply template="work/bulk"/>
       </view>
       
    </bind>
    
    <bind tag="right">
       
        <div class="heading">
          <div class="icon"><img src="/img/main/Messages.png" /></div>
          <div class="content">
            <h1>Messages</h1>
            <h5>Public URL</h5>
          </div>          
        </div> <!-- .heading -->
        
        <div class="subheading">
          <form>
            <textarea></textarea>
            <button type="submit" title="Post Message"></button>
          </form>
        </div> <!-- .subheading -->
        
        <div class="main">
          <div id="messages">
            <div class="message">
              <p>Hello world!</p>
              <div class="info">
                <div class="timestamp"> written 12:01AM, 4.10.2011</div>
                <div class="message-forms">
                  <div class="up"><a href="/vote_up?message=1">+3</a></div>
                  <div class="down"><a href="http://www.weshift.org/vote_down?message=1">-0</a></div>
                  <div class="flag"><a href="http://www.weshift.org/flag?message=1">&nbsp;</a></div>
                </div>
              </div>
            </div>
            <div class="message">
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
            </div>
          </div>
          
          <div id="messages-nav"><a href="#" class="sel">1</a><a href="#">2</a><a href="#" id="next"></a></div>
        </div> <!-- .main -->
    </bind>

</apply>