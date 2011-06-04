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
         <div class="icon sel" id="month" title="Month View"></div>
         <div class="icon-separator"></div>
         <div class="icon" id="day" title="Day View"></div>
         <div class="icon-separator"></div>
         <div class="icon" id="timesheet" title="Timesheet"></div>
         <div class="icon-separator"></div>
         <div class="icon" id="bulk" title="Input Shifts From Spreadsheet"></div>
       </div> <!-- .subheading -->

       <div class="main" id="month">
         <h2>Monthly Calendar</h2>
         <div id="month-heading">
           <h1><div id="prev"></div> May 2011  <div id="next"></div></h1>
         </div> <!-- #month-heading -->
         <div id="month-daynames">
           <div class="dayname">Sun</div>
           <div class="dayname">Mon</div>
           <div class="dayname">Tue</div>
           <div class="dayname">Wed</div>
           <div class="dayname">Thu</div>
           <div class="dayname">Fri</div>
           <div class="dayname">Sat</div>
         </div> <!-- #month-daynames -->
         <div id="month-days">
           <div class="daybox top start"><div class="day">1</div></div>
           <div class="daybox top"><div class="day self">2</div></div>
           <div class="daybox top"><div class="day other">3</div></div>
           <div class="daybox top"><div class="day self">4</div></div>
           <div class="daybox top"><div class="day self">5</div></div>
           <div class="daybox top"><div class="day self">6</div></div>
           <div class="daybox top end"><div class="day">7</div></div>
           <div class="daybox start"><div class="day other">8</div></div>
           <div class="daybox"><div class="day other">9</div></div>
           <div class="daybox"><div class="day other">10</div></div>
           <div class="daybox"><div class="day self">11</div></div>
           <div class="daybox"><div class="day self">12</div></div>
           <div class="daybox"><div class="day self">13</div></div>
           <div class="daybox end"><div class="day">14</div></div>
           <div class="daybox start"><div class="day other">15</div></div>
           <div class="daybox"><div class="day self">16</div></div>
           <div class="daybox"><div class="day self">17</div></div>
           <div class="daybox"><div class="day self">18</div></div>
           <div class="daybox"><div class="day other">19</div></div>
           <div class="daybox"><div class="day">20</div></div>
           <div class="daybox end"><div class="day">21</div></div>
           <div class="daybox start"><div class="day other">22</div></div>
           <div class="daybox"><div class="day other">23</div></div>
           <div class="daybox"><div class="day other">24</div></div>
           <div class="daybox"><div class="day other">25</div></div>
           <div class="daybox"><div class="day other">26</div></div>
           <div class="daybox"><div class="day other">27</div></div>
           <div class="daybox end"><div class="day">28</div></div>
           <div class="daybox bottom start"><div class="day self">29</div></div>
           <div class="daybox bottom"><div class="day self">30</div></div>
           <div class="daybox bottom"><div class="day self">31</div></div>
           <div class="daybox bottom"><div class="day none">&nbsp;</div></div>
           <div class="daybox bottom"><div class="day none">&nbsp;</div></div>
           <div class="daybox bottom"><div class="day none">&nbsp;</div></div>
           <div class="daybox bottom end"><div class="day none">&nbsp;</div></div>
         </div> <!-- .days -->

       </div> <!-- .main -->
       
       <div class="main" id="day">
         <h2>Daily Calendar</h2>
         
       </div>
       <div class="main" id="timesheet">
         <h2>Timesheet | Total Hours: 14.</h2>
           <form><div id="timesheet"><input type="hidden" name="place" value="1">
             <input type="text" name="start" value="2011-5-1"> 
             to 
             <input type="text" name="stop" value="2011-5-30">
             for
             <span><input type="hidden" name="user" value="1"><input disabled="disabled" value="Daniel Patterson" /></span>
             <button type="submit" title="Get Timesheet" /></div></form>
           <table id="entries"><tbody><tr class="heading">
             <td id="hours"><h5>Hours Worked</h5></td>
             <td id="shift"><h5>Original Shift</h5></td>
             <td id="changes"><h5>History</h5></td>
           </tr><tr>
             <td class="entry-hours">7</td>
             <td class="entry-shift">10:30AM-5:30PM<br>May 10, 2011</td>
             <td class="entry-changes"><div class="change">To 5:00AM-11:00AM by<br>Daniel Patterson<br>(11:04PM, 5.30.2011)</div></td>
           </tr><tr>
             <td class="entry-hours">0</td>
             <td class="entry-shift">5:00AM-10:00AM<br>May 11, 2011</td>
             <td class="entry-changes"><div class="delete">Deleted by Daniel Patterson<br>(11:04PM, 5.30.2011)</div></td>
           </tr><tr>
             <td class="entry-hours">7</td>
             <td class="entry-shift">6:45AM-1:45PM<br>May 13, 2011</td>
             <td class="entry-changes"></td>
           </tr></tbody></table>
       </div>
       <div class="main" id="bulk">
         <h2>Bulk Input</h2>
         <form>
           <textarea></textarea>
           <button type="submit" title="Load Data"></button>
         </form>
         <p><strong>Important:</strong> What you paste in should look like the following. It is a format called CSV, which most spreadsheet programs can save as / export to.</p>
         <code>Name,5/2/2011,5/3/2011,5/4/2011,5/5/2011,5/6/2011<br>
         Jane Doe,7-3pm,7-3pm,off,7-3pm,7-3pm<br>
         Fishy Jones,11-7pm,10-4,11-7pm,11-7pm,11-7pm</code>
         <p>WeShift will attempt to read all the shifts, and any that it cannot read it will ignore, which means you can put anything you like (or nothing at all) in days that are off.</p>  
         <p>You will be able to see the shifts that it could read and confirm before they are actually added.</p>        
       </div>
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