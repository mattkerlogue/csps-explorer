const org_select_form = document.getElementById("org-select-form");
const org_select = document.getElementById("org-select");
const go_button = document.getElementById("org-results-go");

go_button.onclick = (e) => {
    if (org_select.value === "") {
        org_select_form.action = "";
    } else if (org_select.value === "2024/CABOFF") {
        let org_url = org_select.value + ".html"
        window.location.assign(org_url);
    } else {
        window.alert("Results not yet available")
    };
};