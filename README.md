# Interactive Map (leaflet)
MoveApps

Github repository: *github.com/movestore/leaflet_mapper*

## Description
Interactive Shiny Leaflet map showing track lines and points coloured by track ID. By clicking on each point a pop-up is displayed containing the values of user-selected attributes.

## Documentation
This app visualises movement tracks as points and lines, coloured by track ID. When the App is first run, it automatically saves an initial HTML map for all tracks as an artifact. By clicking a point a pop-up is shown with the values of the track ID, timestamp, latitude, and longitude. Users can then choose tracks and additional attributes to display in the point popup and apply changes using the Apply Changes button.   
Background map(Basemaps) can be switched between OpenStreetMap, TopoMap, and Aerial. To only display of lines or points can be chosen via the checkbox.     
The App is designed to be responsive for typical datasets (<50,000 locations) and can handle larger inputs (up to ~200,000 locations) depending on device performance. If your data has a larger amount of locations, consider using the App "Rasterized Tracks or Locations on Map".   
The map can be saved locally as HTML or PNG using the Save Map as HTML and Save Map as PNG buttons.

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. 

#### Required data properties
The App should work for any kind of (location) data.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts

`autosave_leaflet_mapper.html`: Initial HTML map for all tracks saved to the output folder. Clicking a point shows a pop-up with the values of the track ID, timestamp, latitude, and longitude.

The following files can be downloaded optionally (to disk):    

`LeafletMap_**.html` : HTML map of the selected tracks (lines) and points, including any user-selected popup attributes.

`LeafletMap_**.png` : PNG image of the selected tracks (lines) and points.


### Settings
**"Tracks"**: select one or multiple individuals. Buttons available to select or unselect all tracks.If no track is selected, a red warning (“No track selected”) is shown and the map is not updated.

**"Attribute"**: Select the attributes to display in the point popup. If nothing is selected, the popup shows only track ID, timestamp, latitude, and longitude.

**"Download"**:  
`Save Map as HTML`: locally downloads the current map in HTML format.  
`Save Map as PNG`:  locally downloads the current map in PNG format.


### Changes in output data

The input data remains unchanged and is passed on as output.

### Null or error handling

**Big data:** If the input data set exceeds 200,000 locations the Shiny UI does not perform properly. Please thin your data for visualisation with this App or use another App to visualize your data.   
**Track Selection** If no track is selected, a red warning (“No track selected”) is shown and the map is not updated.  
**Attribute availability**: The attribute dropdown is updated based on the currently selected tracks and only lists attributes that contain at least one non-missing value for those tracks.  
