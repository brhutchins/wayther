# wayther
A weather bar module for [Waybar](https://github.com/Alexays/Waybar) (or any status bar that supports JSON data), using data from the [OpenWeatherMap](https://openweathermap.org/) API.

Wayther is written in Haskell[*](#haskell), and uses the Aeson library to parse JSON data from the OpenWeatherMap API.

## Building
Build dependencies are managed with [Nix](https://nixos.org/). If you're using direnv, just `direnv allow` the directory. Otherwise, run `nix-shell`. The `pure` flag is only necessary if you already have Haskell/Cabal installed, in which case the local packages can end up getting overridden by the global ones.

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

The executable will be buried somewhere in the `dist-newstyle` directory tree, as is Cabal's wont. Copy the `wayther` executable to somewhere sensible, like `~/.local/bin`, to install it.

## Configuration
Wayther looks in `$HOME/.config/` and `~/.config/` for `wayther/config.json`.

The config file has the following format:

``` json
{
    "api":
        {
            "api_key": "<OpenWeatherMap api key>",
            "location": "<{city},{state},{country code}>"
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

## To do
- [ ] Add a Fahrenheit option
- [ ] Add more information in the tooltip
  - There's a lot more to get out of the API, and making use of that was pretty much the whole point
- [ ] Use `geoclue2` to get location automatically
  
## Haskell
Couldn't this have been written much more easily in, say, Python? Uh huh. With the right API, `jq` would have probably been able to do the job. But everyone ✨*deserves*✨ a monadic, type-safe weather bar module.
