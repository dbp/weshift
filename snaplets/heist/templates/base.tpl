<html>
  <head>
    <link rel="stylesheet" href="/css/base.css" type="text/css" media="screen" title="no title" charset="utf-8">
    <link rel="stylesheet" href="/css/place.css" type="text/css" media="screen" title="no title" charset="utf-8">
    <link rel="stylesheet" href="/css/calendar.css" type="text/css" media="screen" title="no title" charset="utf-8">
    <link rel="stylesheet" href="/css/index.css" type="text/css" media="screen" title="no title" charset="utf-8">
    <!-- <link rel="stylesheet" href="/css/tipTip.css" type="text/css" media="screen" title="no title" charset="utf-8">
    <link rel="stylesheet" href="/css/jquery-ui-1.8.11.custom.css" type="text/css" media="screen" title="no title" charset="utf-8"> -->
    
    <!-- <script type="text/javascript" charset="utf-8" src="/js/jquery-1.5.2.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/jquery.tipTip.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/jquery.bt.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/jquery-ui-1.8.11.custom.min.js"></script> -->


    <script type="text/javascript" charset="utf-8" src="/js/valentine.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/qwery.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/reqwest.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/ready.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/bean.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/bonzo.min.js"></script>
    
   <script type="text/javascript" charset="utf-8" src="/js/heist-async.js"></script>
      
   <script type="text/javascript" charset="utf-8" src="/js/declarative.js"></script>
   <script type="text/javascript" charset="utf-8" src="/js/weshift.js"></script>
  
      
  </head>
  <body>
    <div id="top">
      <div id="top-holder">
        <div id="logo">
          <a href="/"><img src="/img/logo.png" /></a>
          <h2><a href="${placeRoot}"><placeName/></a></h2>
        </div>
        <div id="top-right">
          <ifLoggedIn>
            <userName/> - <a href="/logout?redirectTo=${placeRoot}">Logout</a>
          </ifLoggedIn>
          <ifGuest>
            <apply template="login-form">
              <bind tag="ifPlaces"></bind>
            </apply>
          </ifGuest>
        </div>
      </div> <!-- #top-holder -->
    </div> <!-- #top -->
    <div id="middle">
      <div-async name="middle-left" id="left">
  
      <left/>
      
      </div-async> <!-- #left -->
      <div id="center">
        
      <center/>
 
      </div> <!-- #center -->
      <div id="right">
     
      <right/>
             
      </div> <!-- #right -->
    </div> <!-- #middle -->
    <div id="bottom">
    <div id="footer-wrap">
      <div id="footer">
        <a href="mailto:help@weshift.org/">Contact Us</a>
        <a target="_blank" href="http://positionstudios.com/"><img src="/img/position.png" alt="WeShift by Position Studios"></a>
      </div>
    </div>
    </div> <!-- #bottom -->
    <div id="preload">
      <img src="/img/messages/Message_post_mo.png" />
      <img src="/img/messages_next_mo.png" />
      <img src="/img/message_flag_mo.png" />
      <img src="/img/message_down_mo.png" />
      <img src="/img/message_up_mo.png" />
      <img src="/img/messages/Message_post_mo.png" />
      <img src="/img/workschedule/calendar/Arrow_right_mo.png" />
      <img src="/img/workschedule/calendar/Arrow_left_mo.png" />
      
      <img src="/img/profile/Profile_UserSettings/delete_mo.png" />
      <img src="/img/profile/Profile_UserSettings/email_mo.png" />
      <img src="/img/profile/Profile_UserSettings/password_mo.png" />
      <img src="/img/profile/Profile_UserSettings/name_mo.png" />
      <img src="/img/workschedule/Work_Bulk_mo.png" />
      <img src="/img/workschedule/Work_Timesheet_mo.png" />
      <img src="/img/workschedule/Work_Day_mo.png" />
      <img src="/img/workschedule/Work_Month_mo.png" />
      <img src="/img/profile/Profile_Help_mo.png" />
      <img src="/img/profile/Profile_Coworkers_mo.png" />
      <img src="/img/profile/Profile_UserSettings_mo.png" />

      <img src="/img/profile/Profile_UserSettings/delete_sel.png" />
      <img src="/img/profile/Profile_UserSettings/email_sel.png" />
      <img src="/img/profile/Profile_UserSettings/password_sel.png" />
      <img src="/img/profile/Profile_UserSettings/name_sel.png" />
      <img src="/img/workschedule/Work_Bulk_sel.png" />
      <img src="/img/workschedule/Work_Timesheet_sel.png" />
      <img src="/img/workschedule/Work_Day_sel.png" />
      <img src="/img/workschedule/Work_Month_sel.png" />
      <img src="/img/profile/Profile_Help_sel.png" />
      <img src="/img/profile/Profile_Coworkers_sel.png" />
      <img src="/img/profile/Profile_UserSettings_sel.png" />

      
      <img src="/img/main/SubmitArrow_mo.png" />
      <img src="/img/main/Processing.gif" />
      <img src="/img/Loading.gif" />
    </div>
  </body>
</html>
