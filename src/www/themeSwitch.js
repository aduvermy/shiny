/* File containing the code for the dark and light themes switch */

// Creating constant containing the paths of the css theme files
const themes = {
    dark: 'shinythemes/css/darkly.min.css',
    light: 'shinythemes/css/flatly.min.css'
}

// Creating a new link element
function newLink(theme) {
    let elt = document.createElement('link');
    elt.setAttribute('rel', 'stylesheet');
    elt.setAttribute('text', 'text/css');
    elt.setAttribute('href', theme);
    return elt;
}

// Removing link of current <href> theme
function removeLink(theme) {
    let elt = document.querySelector(`link[href='${theme}']`);
    return elt.parentNode.removeChild(elt);
}

// Defining useful variables 
const darkTheme = newLink(themes.dark);
const lightTheme = newLink(themes.light);
const head = document.getElementsByTagName('head')[0];
const toggle = document.getElementById('themeToggle');

// Defining extra CSS and setting as default
const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info { color: white!important; } .paginate_button { background: white!important; } thead { color: white; }'
const extraDarkThemeElement = document.createElement('style');
extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
head.appendChild(extraDarkThemeElement);


// Defining event : checkbox unchecked (default) == dark theme
toggle.addEventListener('input', function(event) {
    // If toggle not checked, switch to dark theme
    if (!toggle.checked) {
        removeLink(themes.light);
        head.appendChild(extraDarkThemeElement);
        head.appendChild(darkTheme);
    }  else {
        // Else switch to light theme
        removeLink(themes.dark);
        head.removeChild(extraDarkThemeElement)
        head.appendChild(lightTheme);
    }
})
