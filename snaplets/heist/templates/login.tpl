<apply template="base">
  
  <bind tag="center">
    <h5>Login to <placeName/></h5>
      <form method="post" action="/login?redirectTo=$(placeRoot)&pl=$(placeId)"> <!-- $(url) -->
        Name: <input type="text" name="name"><br>
        Password: <input type="password" name="password">
        <input type="submit" value="Login">
      </form>
 </bind>

</apply>