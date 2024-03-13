# sh4worldmeshimport

A Blender 4.0.2 import plugin for Silent Hill 4 PC world meshes.

This does *not* support PlayStation 2 or Xbox due to platform differences, but may in the future.

# Usage

Install the plugin.py file in Blender Preferences --> Add-Ons. Now you should see a *Silent Hill 4 World Mesh (.bin)* option appear in File --> Import.

Once installed, select a .bin file in <SH4 Install Directory>/Data and you should see the parts appear automatically.

# Limitations / Known Issues

- Vertex colors do not yet import properly

- The plugin is hardcoded to work on Blender 4.0.2, but this can be changed to versions other than that by modifying `"blender": (4, 0, 2),` in plugin.py to something else. Blender 3.x versions have been tested and work fine

- World meshes do not include animated objects, such as the fan in Henry's apartment or the death machine in the final boss fight. These are separate meshes and where they should be placed is hardcoded in the exe, they're not present in the world mesh itself, so there is no way to support automatic import and placement of these

- Certain test stages (iwa_teststage.bin for example) have world meshes in a format not used by the final game itself, so these can not be imported *yet*

- Some shadow maps will not blend/display correctly and will just appear black. Hiding the overdraw group in Blender will disable most shadow maps if you're looking to hide the shadows

- Data is imported *exactly* how it is laid out in the data files themselves, therefore you might see oddities that only show up in Blender that wouldn't appear in-game

- The Blender nodes used to draw materials use PBR, but Silent Hill 4 does not use PBR and also doesn't have things like normal/specular/bump maps, therefore some materials will appear too shiny or glossy and might need tweaking to appear correctly if you're trying to make game-accurate renders

# Planned Features

- Importing of light data and placed lights in Blender so the rendered preview in Blender would more closely match how the scene would appear in SH4 itself
- Refactoring and code improvements