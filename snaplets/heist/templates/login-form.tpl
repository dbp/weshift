<div-async name="login-form">
  <form-async action="/login" method="POST">
    Name: 
    <!-- <div class="errors"><name-errors><error/><br></name-errors></div> -->
    <input type="text" name="name" value="${name-value}" />
    Password: 
    <!-- <div class="errors"><password-errors><error/><br></password-errors></div> -->
    <input type="password" name="password" value="${password-value}" />
    <button type="submit"/>
    <ifPlaces>
    <br>
    Which place are you logging into?
    <select name="pl-ajax">
    <places>
    	<option value="${id}"><name/>, <org/></option>
    </places>
    </select>
    </ifPlaces>
    <show notblank="${message}">
    	<h5><message/></h5>
    </show>
  </form-async>
</div-async>
