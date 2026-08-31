// Fix relative image paths in bilingual text notes (双语笔记图片修复)
// Reads the #docPath label on the active note, rewrites relative <img src> to
// the bundled help docs resource path so images render in the desktop app.
//
// Setup: put this in a code note with mime "application/javascript;env=frontend"
// and add the label #run=frontendStartup (manual step, cannot be set via unote MCP).

const PREFIX = () => (window.glob && window.glob.assetPath) || 'assets/v0.105.0';

function activeDocPath() {
    try {
        const note = api.getActiveContextNote();
        if (!note) return null;
        if (typeof note.getLabelValue === 'function') {
            return note.getLabelValue('docPath');
        }
        const attrs = note.getAttributes ? note.getAttributes() : [];
        for (const a of attrs) {
            if (a.type === 'label' && a.name === 'docPath') return a.value;
        }
        return null;
    } catch (e) {
        return null;
    }
}

function fixImages() {
    const docPath = activeDocPath();
    if (!docPath) return;
    const lastSlash = docPath.lastIndexOf('/');
    if (lastSlash < 0) return;
    const docDir = docPath.substring(0, lastSlash);
    const prefix = `${PREFIX()}/doc_notes/en/${docDir}/`;

    document.querySelectorAll('.ck-content img, .note-detail-content img, .content-rendered img').forEach((img) => {
        const src = img.getAttribute('src');
        if (!src) return;
        if (/^(https?:|data:|blob:|#|\/|api\/)/i.test(src)) return;
        if (src.indexOf('assets/') === 0 || src.indexOf('doc_notes/') === 0) return;
        const clean = src.split('#')[0].split('?')[0];
        img.setAttribute('src', prefix + clean);
    });
}

let fixTimer = null;
function scheduleFix() {
    if (fixTimer) clearTimeout(fixTimer);
    fixTimer = setTimeout(fixImages, 150);
}

const observer = new MutationObserver(scheduleFix);
observer.observe(document.body, { childList: true, subtree: true });

// Run once at startup and periodically (covers any missed render)
setTimeout(fixImages, 400);
setInterval(fixImages, 2500);
