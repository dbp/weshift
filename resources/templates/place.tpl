<apply template="base">

<bind tag="left">
        
        <div class="heading">
          <div class="icon"><img src="/img/main/Profile.png" /></div>
          <div class="content">
            <h4>Daniel Patterson</h4>
            <h5>Position, Providence</h5>
          </div> <!-- .content -->
        </div> <!-- .heading -->
        
        <div class="subheading">
          <div class="icon sel" id="usersettings" title="User Settings"></div>
          <div class="icon-separator"></div>
          <div class="icon" id="coworkers" title="Coworkers"></div>
          <div class="icon-separator"></div>
          <div class="icon" id="help" title="Help"></div>
        </div> <!-- .subheading -->
        
        
        <div class="main">
          <h2>User Settings</h2>
          <div class="subheading">
            <div class="icon" id="name" title="Change Name"></div>
            <div class="icon-separator"></div>
            <div class="icon sel" id="password" title="Change Password"></div>
            <div class="icon-separator"></div>
            <div class="icon" id="delete" title="Remove Account"></div>
            <div class="icon-separator"></div>
            <div class="icon" id="email" title="Email Settings"></div>
          </div> <!-- .subheading -->
          <div class="content">
            <h3>Change Password</h3>
            <form>
              <table>
                <tr><td class="label"><label for="current">Current:</label></td> <td><input name="current" type="password" /></td></tr>
                <tr><td class="label"><label for="password">New:</label></td> <td><input name="password" type="password" /></td></tr>
                <tr><td class="label"><label for="confirm">Confirm:</label></td> <td><input name="confirm" type="password" /><button type="submit" title=""/></td></tr>
              </table>
            </form>
          </div> <!-- .content -->
        </div> <!-- #main -->
  
</bind>

    <bind tag="center">
       <div class="heading">
         <div class="icon"><img src="/img/main/WorkSchedule.png" /></div>
         <div class="content">
           <h1>Work Schedule</h1>
           <h5>Next Shift: 9:30am, 28 May 2011</h5>
         </div>
       </div> <!-- .heading -->
       
       <div class="subheading">
         <a-async href="$(placeRoot)/month/2011/5" class="icon sel" id="month" title="Month View"></a-async>
         <div class="icon-separator"></div>
         <a-async href="$(placeRoot)/day/2011/5/1" class="icon" id="day" title="Day View"></a-async>
         <div class="icon-separator"></div>
         <a-async href="$(placeRoot)/timesheet" class="icon" id="timesheet" title="Timesheet"></a-async>
         <div class="icon-separator"></div>
         <a-async href="$(placeRoot)/bulk" class="icon" id="bulk" title="Input Shifts From Spreadsheet"></a-async>
       </div> <!-- .subheading -->

       <apply template="work/month_calendar"/>

       
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