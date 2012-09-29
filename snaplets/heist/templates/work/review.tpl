<apply template="heading">
  <bind tag="review_sel">sel</bind>
</apply>

<div-async name="center-main" class="main" id="review">
    <form-async action="${placeRoot}/review" method="POST">
    <div id="review">
      <input class="date" type="text" name="start" value="${reviewStart}"> 
      to 
      <input class="date" type="text" name="stop" value="${reviewStop}">
      <button type="submit" title="Get Review" /></div></form-async>
    <table id="claims"><tbody><tr class="heading">
      <td id="claim"><h5>Claim</h5></td>
      <td id="units"><h5>Units</h5></td>
      <td id="shift"><h5>Shift</h5></td>
      <td id="claimer"><h5>Claimer</h5></td>
      <td id="claimee"><h5>Claimee</h5></td>
      <td id="resolve"><h5>Resolve</h5></td>
    </tr>
    
    <unresolvedClaims>
      <tr>
        <td class="claim-description"><reason/></td>
        <td class="claim-units"><units/> of <shift><units/></shift></td>
        <td class="claim-shift"><shift><start/><notDeadline>-<stop/></notDeadline> on <date/></shift></td>
        <td class="claim-claimer"><user-lookup id="${user}"><name/></user-lookup></td>
        <td class="claim-claimee"><shift><user-lookup id="${user}"><name/></user-lookup></shift></td>
        <td class="claim-resolve">
          <a-async href="${placeRoot}/shift/claim/${id}/accept">Accept</a-async>
          <a-async href="${placeRoot}/shift/claim/${id}/cancel">Cancel</a-async>
        </td>
      </tr>
    </unresolvedClaims>
    
    </tbody></table>
    
    <table id="changes"><tbody><tr class="heading">
        <td id="user"><h5>User</h5></td>
        <td id="shift"><h5>Shift</h5></td>
        <td id="changes"><h5>Changes</h5></td>
      </tr>
      <userModifiedShifts>
        <tr>
          <td class="change-user"><shift><user-lookup id="${user}"><name/></user-lookup></shift></td>
          <td class="change-shift"><shift><start/><notDeadline>-<stop/></notDeadline> on <date/> (<units/>)</shift></td>
          <td class="change-changes">
            <changes>
              <div class="${changeClasses}"><changeDescription/> by <changePerson/><br>(<changeTime/>, <changeDate/>)</div>
            </changes>
          </td>
        </tr>
      </userModifiedShifts>
    </table>
</div-async>