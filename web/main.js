import init, * as wasm from './web-pack/rustyboi_advance_platform_lib.js';

async function initializeEmulator() {
    try {
        console.log('Initializing RustyBoi Advance WASM module...');
        await init();
        console.log('WASM module loaded successfully');
    } catch (error) {
        console.error('Failed to initialize emulator:', error);
    }
}

// Initialize everything when the page loads
window.addEventListener('DOMContentLoaded', () => {
    console.log('DOM loaded, setting up RustyBoi Advance...');

    // Start initializing the emulator immediately
    initializeEmulator().catch(error => {
        console.error('Failed to start emulator:', error);
    });
});

// Prevent context menu on right-click (common for games)
document.addEventListener('contextmenu', (event) => {
    event.preventDefault();
});

console.log('RustyBoi Advance JavaScript loader initialized');
