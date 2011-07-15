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
    <h2>Create New Place</h2>
    <apply template="signup_form"/>
  </bind> 
</apply>
