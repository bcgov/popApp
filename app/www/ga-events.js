// ga-events.js

document.addEventListener('DOMContentLoaded', function () {
  if (window.Shiny) {
    Shiny.addCustomMessageHandler('trackDownload', function (message) {
      if (typeof gtag === 'function') {
        gtag('event', 'file_download', {
          event_category: 'file',
          event_label: message.filename || 'shiny_download',
          transport_type: 'beacon'
        });
      }
    });
  } else {
    console.warn('Shiny not found: Shiny download tracking disabled');
  }
});
