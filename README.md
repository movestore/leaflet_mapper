# Interactive Map (leaflet)
MoveApps

Github repository: *github.com/movestore/leaflet_mapper*

## Description
Shiny leaflet map of points and line with ID colour. Background can be selected from aerial or topographical satellite images. IDs can be shown/hidden. Proper performance for up to 200,000 locations, comfortable performance less than 50,000 locations.

## Documentation
This App creates a Shiny UI that allows the interactive exploration of multiple tracks in a map. The leaflet map can be zoomed, the background openstreetmap can be selected as `TopoMap`or `Aerial` and the tracks of each animal can be selected or unselected. Each animal's tracks consist of points and lines and have a different colour (indicated on the top right).

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format
Shiny user interface (UI)

### Artefacts
none

The button `Save Plot` in the UI is supposed to allow the local download of the created map.

### Settings 
There are no parameters/settings, but the App opens into an interactive User Interface (UI) where settings can be adapted.

### Null or error handling:
**Data:** For use in further Apps the input data set is returned. Empty input will give an error.

**Big data:** If the input data set exceeds 200,000 locations the Shiny UI does not perform properly. Please thin your data for visualisation with this App.
