function changeTab(tabName,elmnt) {
    var i, tabviz,  tablinks;

    // Hide all elements with class="tabviz" by default */
    tabviz = document.getElementsByClassName("tabviz");
    for (i = 0; i < tabviz.length; i++) {
	    tabviz[i].style.display = "none";
    }

    // Remove the background color of all tablinks/buttons
    tablinks = document.getElementsByClassName("tablink");
    for (i = 0; i < tablinks.length; i++) {
	    tablinks[i].classList.remove('button-outline');
    }

    // Show the specific tab content
    document.getElementById(tabName).style.display = "block";

    // Add the specific color to the button used to open the tab content
    elmnt.classList.add('button-outline');
    $.sparkline_display_visible()

}

// Get the element with id="defaultOpen" and click on it
document.getElementById("defaultOpen").click();
