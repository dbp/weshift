<hasClaims>
  <ul class="claims">
    <claims id="${id}">
      <li>
        <div-async name="claim-${id}">
          <resolved><accepted>Accepted</accepted><notAccepted>Cancelled</notAccepted>:</resolved>
          <user-lookup id="${user}"><name/></user-lookup>
          <resolved>claimed</resolved>
          <notResolved>claims</notResolved>
          <units/> units because "<reason/>"
          <notResolved>
            <isNormalUser>
              <show notblank="${claimee}">
                  <a-async href="${placeRoot}/shift/claim/${id}/accept">Accept</a-async>
              </show>
              <userClaim><a-async href="${placeRoot}/shift/claim/${id}/cancel">Cancel</a-async></userClaim>
            </isNormalUser>
            <isFacilitator>
              <a-async href="${placeRoot}/shift/claim/${id}/accept">Accept</a-async>
              <a-async href="${placeRoot}/shift/claim/${id}/cancel">Cancel</a-async>
            </isFacilitator>
          </notResolved>
        </div-async>
      </li>
    </claims>
  </ul>
</hasClaims>