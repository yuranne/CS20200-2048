# CS20200 2048

SAFE-style F# 2048 web service built with Fable, Elmish, Feliz, Vite, Tailwind, Giraffe/Fable Remoting, and SQLite.

## Live demo

### Play the hosted version on Render: [https://cs20200-2048.onrender.com/](https://cs20200-2048.onrender.com/)
<br />

## Requirements

- .NET SDK 10.0.103 or compatible .NET 10 SDK
- Node.js 22+
- npm 10+

## Development

```powershell
npm run restore
npm run dev:server
npm run dev:client
```

Open the Vite client at `http://localhost:5173`. The client proxies `/api` calls to the F# server on `http://localhost:5000`.

## Build and test

```powershell
npm run build
npm test
```

The production client is emitted into `src/Server/wwwroot` and served by the F# server.

## Docker

```powershell
docker compose up --build
```

The app listens on `http://localhost:8080` and stores SQLite data in the `leaderboard-data` volume.
