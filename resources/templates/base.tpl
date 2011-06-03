<html>
  <head>
    <title>WeShift</title>
  </head>
  <body>
    <div id="navigations">
      <ifLoggedIn>
        <a href="/logout">logout</a> | <a href="place">place</a>
      </ifLoggedIn>
      <ifGuest>
         <a href="login">login</a> | <a href="signup">signup</a>
      </ifGuest>
    </div> <!-- #navigation -->

    
    <content />
  </body>
</html>