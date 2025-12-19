# Interactive Map (leaflet)
MoveApps

Github repository: *github.com/movestore/leaflet_mapper*

## Description
Interactive Shiny Leaflet map showing track lines coloured by track ID and points with user-selected attributes for the selected tracks.

## Documentation

This app visualises movement tracks as points and lines, coloured by track ID.  
When the app is first run, it automatically saves an initial HTML map for all tracks to the output folder; clicking a point shows the track ID, timestamp, latitude, and longitude.  
Users can then choose tracks and additional attributes to display in the point popup and apply changes using the Apply Changes button.   
Background map(Basemaps) can be switched between OpenStreetMap, TopoMap, and Aerial, and points-lines can be shown on-off via the layer control.  
The app is designed to be responsive for typical datasets (<50,000 locations) and can handle larger inputs (up to ~200,000 locations) depending on device performance.    
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
This App produces the following HTML file as an artefact:  
`autosave_leaflet_mapper.html`: Initial HTML map for all tracks saved to the output folder. Clicking a point shows the track ID, timestamp, latitude, and longitude.

The following files can be downloaded optionally:    

`LeafletMap_**.html` : HTML map of the selected tracks (lines) and points, including any user-selected popup attributes.

`LeafletMap_**.png` : PNG image of the selected tracks (lines) and points.


### Settings
**"Tracks"**: select one or multiple individuals. Buttons available to select or unselect all tracks.

**"Attribute"**: Select the attributes to display in the point popup. If nothing is selected, the popup shows only track ID, timestamp, latitude, and longitude.

**"Download"**:  
`Save Map as HTML`: locally downloads the current map in HTML format.  
`Save Map as PNG`:  locally downloads the current map in PNG format.


### Changes in output data

The input data remains unchanged and is passed on as output.

### Null or error handling
**Data:** For use in further Apps the input data set is returned. Empty input will give an error.

**Big data:** If the input data set exceeds 200,000 locations the Shiny UI does not perform properly. Please thin your data for visualisation with this App. Note that performance of MoveApps has recently improved, try out if you need to plot more locations.  

**Missing attributes:** If a selected attribute contains only missing values for the selected tracks, it will not be shown in the popup.

