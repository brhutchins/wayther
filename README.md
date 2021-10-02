# wayther
A weather bar module for [Waybar](https://github.com/Alexays/Waybar) (or any status bar that supports JSON data), using data from the [OpenWeatherMap](https://openweathermap.org/) API.

Wayther is written in Haskell, and uses the Aeson library to parse JSON data from the OpenWeatherMap API.

## Building
Build dependencies are managed with Nix. If you're using nix-direnv, just `direnv allow` the directory. Otherwise, run `nix-shell`. The `pure` flag is only necessary if you already have Haskell/cabal installed.

``` sh
nix-shell --pure
```

That should provide the build tools and libraries. Then run `hpack` to generate the cabal file for building:

``` sh
hpack
```

Then build:

``` sh
cabal build
```

The executable will be buried somewhere in the `dist-newstyle` directory tree, as is cabal's wont. Copy the `wayther` executable to somewhere like `~/.local/bin` (or wherever) to install it.

## Configuration
Wayther looks in `$HOME/.config/` and `~/.config/` for `wayther/config.json`.

The config file has the following format:

``` json
{
    "api":
        {
            "api_key": "<OpenWeatherMap api key>",
            "location": "<{city},{state},{country code}>",
        }
}
```

Both `api_key` and `location` are necessary. The free tier key at OpenWeatherMap works fine for the API key.

## Waybar configuration
Run as a custom type in Waybar (e.g., `custom/weather`). Setup in Waybar config is as follows:

``` json
"custom/weather": {
    "return-type": "json",
    "exec": "<PATH/TO/EXECUTABLE>",
    "interval": 600
}
```

