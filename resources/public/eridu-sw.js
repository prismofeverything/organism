// Eridu service worker — caches the offline-mode page + bundle so the
// tablet can play with airplane mode on. POSTs to /eridu/offline/log
// are passed through to the network so the existing localStorage queue
// in play.cljs handles offline-vs-online state itself.

// v2: scope the SW to Eridu's OWN assets only. Previously this handler cached
// ANY same-origin GET on demand at scope "/", so it hijacked the whole origin
// (Journeymen, Chroma, Journey, …) and served those apps STALE bundles
// stale-while-revalidate — bypassing the server's no-store headers. That made
// the phone load an old Journeymen bundle forever while headless (no SW) saw
// the fresh one. Bumping the version also purges the old over-broad cache.
const CACHE_VERSION = 'v2';
const CACHE_NAME    = `eridu-${CACHE_VERSION}`;

// Best-effort precache. The fetch handler caches Eridu-owned assets on demand.
const PRECACHE_URLS = [
  '/eridu/offline',
  '/js/eridu.js',
  '/css/screen.css',
  '/assets/bulma/css/bulma.min.css',
  '/assets/material-icons/css/material-icons.min.css',
  '/favicon.ico',
  '/eridu-manifest.json'
];

self.addEventListener('install', (event) => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then((cache) => Promise.all(
        // addAll is all-or-nothing; do them individually so one bad URL
        // doesn't kill the whole precache step.
        PRECACHE_URLS.map((u) =>
          cache.add(u).catch((e) => console.warn('SW precache miss', u, e))
        )
      ))
      .then(() => self.skipWaiting())
  );
});

self.addEventListener('activate', (event) => {
  event.waitUntil(
    caches.keys()
      .then((keys) => Promise.all(
        keys.filter((k) => k !== CACHE_NAME).map((k) => caches.delete(k))
      ))
      .then(() => self.clients.claim())
  );
});

self.addEventListener('fetch', (event) => {
  const req = event.request;

  // Only handle GET — POSTs to /eridu/offline/log etc. go to network as-is.
  if (req.method !== 'GET') return;

  const url = new URL(req.url);
  if (url.origin !== location.origin) return;

  // Sync endpoint: always network. The cljs queue handles failures.
  if (url.pathname.startsWith('/eridu/offline/log')) return;

  // Scope guard: this SW is registered at "/" (so it can control the Eridu PWA
  // start_url) but it must ONLY cache/serve Eridu's own offline assets. Any
  // other app's request — notably the Journeymen / Chroma / Journey JS bundles
  // — passes straight through to the network so those apps always get the live,
  // freshly-built file. Without this, stale-while-revalidate served the phone an
  // old Journeymen bundle indefinitely.
  const p = url.pathname;
  const isEriduAsset =
    p.startsWith('/eridu') ||
    p === '/js/eridu.js' ||
    PRECACHE_URLS.indexOf(p) !== -1 ||
    p.startsWith('/assets/') ||      // shared bulma/material-icons (eridu offline css)
    p === '/css/screen.css';
  if (!isEriduAsset) return;         // network-only for everything non-Eridu

  // Stale-while-revalidate: serve cached if present, refresh in background.
  // ignoreSearch so /js/eridu.js?v=N matches /js/eridu.js?v=M from cache.
  event.respondWith(
    caches.match(req, { ignoreSearch: true }).then((cached) => {
      const networkFetch = fetch(req)
        .then((res) => {
          // Only cache same-origin successful basic responses.
          if (res && res.ok && res.type === 'basic') {
            const clone = res.clone();
            caches.open(CACHE_NAME).then((cache) => cache.put(req, clone));
          }
          return res;
        })
        .catch(() => null);

      if (cached) {
        // Kick off background refresh, return cached immediately.
        networkFetch.catch(() => {});
        return cached;
      }
      // No cache — try network, fall back to a friendly offline notice.
      return networkFetch.then((res) =>
        res || new Response(
          'Offline and not yet cached. Reconnect once to populate the cache.',
          { status: 503, statusText: 'Offline', headers: { 'Content-Type': 'text/plain' } }
        )
      );
    })
  );
});
