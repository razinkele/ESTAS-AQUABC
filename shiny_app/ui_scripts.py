"""Inline JavaScript blocks for the AQUABC UI (extracted from create_ui())."""
from shiny import ui


def reload_script():
    return ui.tags.script("""
        Shiny.addCustomMessageHandler('reload_page', function(message) {
            console.log('Reloading page:', message);
            setTimeout(function() {
                window.location.reload();
            }, 500);
        });
        
        Shiny.addCustomMessageHandler('copy_to_clipboard', function(text) {
            if (navigator.clipboard && window.isSecureContext) {
                navigator.clipboard.writeText(text).then(function() {
                    console.log('Copying to clipboard was successful!');
                }, function(err) {
                    console.error('Could not copy text: ', err);
                });
            } else {
                // Fallback
                let textArea = document.createElement("textarea");
                textArea.value = text;
                textArea.style.position = "fixed";
                textArea.style.left = "-9999px";
                textArea.style.top = "0";
                document.body.appendChild(textArea);
                textArea.focus();
                textArea.select();
                try {
                    document.execCommand('copy');
                    console.log('Fallback: Copying to clipboard was successful!');
                } catch (err) {
                    console.error('Fallback: Oops, unable to copy', err);
                }
                document.body.removeChild(textArea);
            }
        });
    """)


def nav_script():
    return ui.tags.script("""
        function initSidebar() {
            const toggleBtn = document.getElementById('sidebar-collapse-btn');
            const sidebar = document.getElementById('custom-sidebar');
            const navLinks = document.querySelectorAll('.custom-sidebar .nav-link');
            
            // Toggle sidebar collapsed state
            if (toggleBtn && sidebar) {
                toggleBtn.onclick = function(e) {
                    e.stopPropagation();
                    sidebar.classList.toggle('collapsed');
                };
            }
            
            // Navigation link click handler
            navLinks.forEach(function(link) {
                link.onclick = function(e) {
                    e.preventDefault();
                    // Update active states
                    navLinks.forEach(function(l) { l.classList.remove('active'); });
                    link.classList.add('active');
                    
                    // Update Shiny input value
                    var navId = link.getAttribute('data-nav-id');
                    Shiny.setInputValue('navigation', navId);
                };
            });
        }
        
        // Run on load and after Shiny updates
        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', initSidebar);
        } else {
            initSidebar();
        }
        // Also run after a short delay to catch Shiny's dynamic content
        setTimeout(initSidebar, 500);
    """)


def settings_script():
    return ui.tags.script("""
        $(document).on('click', '#settings_toggle', function() {
            var offcanvas = new bootstrap.Offcanvas(document.getElementById('settingsOffcanvas'));
            offcanvas.toggle();
        });
    """)


def help_script():
    return ui.tags.script("""
        $(document).on('click', '#help_toggle', function() {
            var offcanvas = new bootstrap.Offcanvas(document.getElementById('helpOffcanvas'));
            offcanvas.toggle();
        });
    """)


def changelog_script():
    return ui.tags.script("""
        $(document).on('click', '#changelog_toggle', function() {
            var offcanvas = new bootstrap.Offcanvas(document.getElementById('changelogOffcanvas'));
            offcanvas.toggle();
        });
    """)


def theme_script():
    return ui.tags.script("""
        (function() {
            // Apply saved theme immediately (before DOM paints) to avoid flash
            var saved = localStorage.getItem('aquabc-theme');
            if (saved === 'light') {
                document.documentElement.classList.add('light');
            }

            $(document).ready(function() {
                function updateIcon() {
                    var isLight = document.documentElement.classList.contains('light');
                    var icon = document.getElementById('theme-icon');
                    if (icon) {
                        icon.className = isLight ? 'bi bi-moon-fill' : 'bi bi-sun-fill';
                    }
                }
                updateIcon();

                $(document).on('click', '#theme-toggle-btn', function() {
                    document.documentElement.classList.toggle('light');
                    var isLight = document.documentElement.classList.contains('light');
                    localStorage.setItem('aquabc-theme', isLight ? 'light' : 'dark');
                    updateIcon();
                });
            });
        })();
    """)

