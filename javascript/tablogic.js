function changeTab(tabName,tabNameViz,elmnt) {
    var i, tabcontent, tabviz,  tablinks;
    
    // Hide all elements with class="tabcontent" by default */
    tabcontent = document.getElementsByClassName("tabcontent");
    for (i = 0; i < tabcontent.length; i++) {
	tabcontent[i].style.display = "none";
    }

    // Hide all elements with class="tabviz" by default */
    tabviz = document.getElementsByClassName("tabviz");
    for (i = 0; i < tabviz.length; i++) {
	tabviz[i].style.display = "none";
    }

    // Remove the background color of all tablinks/buttons
    tablinks = document.getElementsByClassName("tablink");
    for (i = 0; i < tablinks.length; i++) {
	tablinks[i].style.backgroundColor = "";
    }

    // Show the specific tab content
    document.getElementById(tabName).style.display = "block";
    document.getElementById(tabNameViz).style.display = "block";

    // Add the specific color to the button used to open the tab content
    elmnt.style.backgroundColor = "#262626";
}

// Get the element with id="defaultOpen" and click on it
document.getElementById("defaultOpen").click(); 
