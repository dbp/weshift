<apply template="heading">
  <bind tag="coworkers_sel">sel</bind>
  <bind tag="coworkers">blank</bind>
  <bind tag="settings">settings</bind>
  <bind tag="help">help</bind>
</apply>

<div-async name="profile-main" class="main">
  <h2>Coworkers (<coworkersCount/>)</h2>

  <apply-content/>

</div-async> <!-- #main -->
