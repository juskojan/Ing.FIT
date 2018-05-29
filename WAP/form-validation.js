/*
 *	Select all text inputs
 *
 */
var inputs = document.querySelectorAll('input[type="text"]');

/*
 *	Initial check of requirements at load-up
 */
checker();

/*
 *	Function that checks al inputs validity.
 *  Colors all inputs accordingly & disables/enables submit button.
 */
function checker() {
	// Are all inputs valid?
	var all_valid = true;

	for (var i = 0; i < inputs.length; i++) {
		// Is this iteration's input valid?
		var validity = true;
		// Have any validation rule been used?
		var rule = false;

		// IF INPUT REQUIRED OR NOT EMPTY
        if(inputs[i].dataset.validationRequired || inputs[i].value.length){

        	// RULE  data-validation-required
        	if(inputs[i].dataset.validationRequired){
            	rule = true;
                validity = validity && inputs[i].value.length;
			}

			// RULE  data-validation-minlength
            if(inputs[i].dataset.validationMinlength){
            	rule = true;
                validity = validity && (inputs[i].value.length >= inputs[i].dataset.validationMinlength);
            }

            // RULE  data-validation-maxlength
            if(inputs[i].dataset.validationMaxlength){
            	rule = true;
                validity = validity && (inputs[i].value.length <= inputs[i].dataset.validationMaxlength);
            }

            // RULE  data-validation-regex
            if(inputs[i].dataset.validationRegex){
            	rule = true;
                var regex = new RegExp(inputs[i].dataset.validationRegex);
                validity = validity && regex.test(inputs[i].value);
            }

            // IF any rule has been applied then color
            if(rule){
                if(validity){
                    inputs[i].style.backgroundColor = "#dcffc9";
                    all_valid = all_valid && true;
                }else{
                    inputs[i].style.backgroundColor = "#ffd4c9";
                    all_valid = all_valid && false;
                }
			}
        }else{
        	// NOT REQUIRED AND EMPTY
            inputs[i].style.backgroundColor = "white";
		}
	}

    if(all_valid){
        document.querySelector('input[type="submit"]').removeAttribute("disabled");
    }else{
        document.querySelector('input[type="submit"]').setAttribute("disabled", "true");
    }
}

// ADD input event listener to our checker function
for (var i = 0; i < inputs.length; i++) {
	inputs[i].addEventListener('input', function() {
		checker();
	});
}