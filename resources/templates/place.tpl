<apply template="base">

<bind tag="left">
      <div class="heading">
         <div class="icon"><img src="/img/main/Profile.png" /></div>
         <div class="content">
           <h3><userName/></h3>
           <h5>
            <form action="$(placeRoot)">
              <select onchange="this.parentNode.action = this.value;">
               <userPlaces>
                <show selected="$(current)">
                  <option value="$(root)" selected="selected"><shortname/></option>
                </show>
                <show notselected="$(current)">
                  <option value="$(root)"><name/>, <org/></option>
                </show>
              </userPlaces>
              </select>
              <button type="submit" value=""/>
            </form>
          </h5>
         </div>
       </div> <!-- .heading -->

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
          <apply template="messages/add_form"/>
        </div> <!-- .subheading -->
        
        <div class="main">
          <apply template="messages/page"/>
        </div> <!-- .main -->
    </bind>

</apply>