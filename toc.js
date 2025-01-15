// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><li class="part-title">User Guide</li><li class="chapter-item expanded "><a href="introduction.html"><strong aria-hidden="true">1.</strong> Introduction</a></li><li class="chapter-item expanded "><a href="using-lambdabuffers.html"><strong aria-hidden="true">2.</strong> Using LambdaBuffers</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="haskell.html"><strong aria-hidden="true">2.1.</strong> LambdaBuffers to Haskell</a></li><li class="chapter-item expanded "><a href="plutustx.html"><strong aria-hidden="true">2.2.</strong> LambdaBuffers to PlutusTx</a></li><li class="chapter-item expanded "><a href="purescript.html"><strong aria-hidden="true">2.3.</strong> LambdaBuffers to Purescript</a></li><li class="chapter-item expanded "><a href="plutarch.html"><strong aria-hidden="true">2.4.</strong> LambdaBuffers to Plutarch</a></li><li class="chapter-item expanded "><a href="rust.html"><strong aria-hidden="true">2.5.</strong> LambdaBuffers to Rust</a></li><li class="chapter-item expanded "><a href="typescript.html"><strong aria-hidden="true">2.6.</strong> LambdaBuffers to Typescript</a></li></ol></li><li class="chapter-item expanded "><li class="part-title">Reference Guide</li><li class="chapter-item expanded "><a href="getting-started.html"><strong aria-hidden="true">3.</strong> Getting started</a></li><li class="chapter-item expanded "><a href="design.html"><strong aria-hidden="true">4.</strong> Design</a></li><li class="chapter-item expanded "><a href="api.html"><strong aria-hidden="true">5.</strong> API</a></li><li class="chapter-item expanded "><a href="syntax.html"><strong aria-hidden="true">6.</strong> LambdaBuffers Frontend (.lbf) syntax</a></li><li class="chapter-item expanded "><a href="compiler.html"><strong aria-hidden="true">7.</strong> Compiler</a></li><li class="chapter-item expanded "><a href="codegen.html"><strong aria-hidden="true">8.</strong> Codegen</a></li><li class="chapter-item expanded "><a href="command-line-interface.html"><strong aria-hidden="true">9.</strong> Command line interface</a></li><li class="chapter-item expanded "><a href="comparison-matrix.html"><strong aria-hidden="true">10.</strong> Comparison matrix</a></li><li class="chapter-item expanded "><a href="aiken-integration.html"><strong aria-hidden="true">11.</strong> Aiken Research Document</a></li><li class="chapter-item expanded "><a href="catalyst-reports.html"><strong aria-hidden="true">12.</strong> Catalyst reports</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="catalyst09-reports/index.html"><strong aria-hidden="true">12.1.</strong> Catalyst 9 reports</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="catalyst09-reports/milestone-1.html"><strong aria-hidden="true">12.1.1.</strong> Milestone 1: Research</a></li><li class="chapter-item expanded "><a href="catalyst09-reports/milestone-2.html"><strong aria-hidden="true">12.1.2.</strong> Milestone 2: End to end proof of concept</a></li><li class="chapter-item expanded "><a href="catalyst09-reports/milestone-3.html"><strong aria-hidden="true">12.1.3.</strong> Milestone 3: Testing and documentation</a></li><li class="chapter-item expanded "><a href="catalyst09-reports/milestone-3.html"><strong aria-hidden="true">12.1.4.</strong> Milestone 4: Project adoption</a></li></ol></li><li class="chapter-item expanded "><a href="catalyst10-reports/index.html"><strong aria-hidden="true">12.2.</strong> Catalyst 10 reports</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="catalyst10-reports/milestone-1.html"><strong aria-hidden="true">12.2.1.</strong> Milestone 1: Rust support</a></li><li class="chapter-item expanded "><a href="catalyst10-reports/milestone-2.html"><strong aria-hidden="true">12.2.2.</strong> Milestone 2: Javascript/Typescript support</a></li><li class="chapter-item expanded "><a href="catalyst10-reports/milestone-3.html"><strong aria-hidden="true">12.2.3.</strong> Milestone 3: Aiken integration research and development</a></li><li class="chapter-item expanded "><a href="catalyst10-reports/milestone-4.html"><strong aria-hidden="true">12.2.4.</strong> Milestone 4: Separate PlutusTx backend and improvements to existing LambdaBuffers facilities</a></li><li class="chapter-item expanded "><a href="catalyst10-reports/milestone-5.html"><strong aria-hidden="true">12.2.5.</strong> Final Milestone: Project scaffold for Rust, JavaScript and Haskell</a></li></ol></li></ol></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString();
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
