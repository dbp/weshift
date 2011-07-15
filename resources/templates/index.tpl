<apply template="base">
  <bind tag="left">
    <h2>Places</h2>
    <places>
      <a href="$(root)"><name/>, <org/></a><br>
    </places>
  </bind>

  <bind tag="center">
      <h2>What is WeShift?</h2>
      <p>
        WeShift was built with and for people who work shifts that change often and have difficulty getting time off. It allows workers to load in their schedules (by hand or via spreadsheet) and make changes to their shifts. They can also request shifts off, in which case their available coworkers are contacted and can elect to cover the shift. All of this is reflected in an annotated timesheet that they can use to check their paychecks for errors.
      </p>
  </bind>
  
  <bind tag="right">
    <h2>Sign up</h2>
    <div-async name="signup-form">
      <form-async target="/signup" method="POST">
        Org: <input type="text" name="org"/><br>
        Place: <input type="text" name="place"/><br>
        Name: <input type="text" name="name"/>
        <button type="submit"/>
      </form-async>
    </div-async>
  </bind> 
</apply>
