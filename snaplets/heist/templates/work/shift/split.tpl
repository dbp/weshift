<div-async name="split-form-${id}" class="split-form-${id} split-form" style="display: ${disp};">
    <dfForm data-async="1" action="${placeRoot}/shift/split" method="POST">
            <dfInputHidden ref="user" data-default="${user}"/>
            <dfInputHidden ref="id" data-default="${id}"/>
            <dfInputHidden ref="day" data-default="${dayNum}"/>
            <dfInputHidden ref="month" data-default="${currMonth}"/>
            <dfInputHidden ref="year" data-default="${currYear}"/>
            <dfInputHidden ref="start" data-default="${start}"/>
            <dfLabel ref="stop">stop at</dfLabel>
            <dfInputText ref="stop" data-default="${stop}"/>
            <dfLabel ref="units">units</dfLabel>
            <dfInputText ref="units" data-default="${units}"/>
            <dfInputHidden ref="color" data-default="${color}"/>
            <dfInputHidden ref="description" data-default="${description}"/>
            <dfChildErrorList class="errors" />
            
            <button type="submit"/>
    </dfForm>
</div-async>