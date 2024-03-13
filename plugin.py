import bpy
import bpy_extras
import bmesh
import math
import struct
import os

bl_info = {
    "name": "Silent Hill 4 World Mesh Import",
    "category": "Import-Export",
    "location": "File > Import",
    "description": "Import a Silent Hill 4 world mesh into the scene.",
    "author": "Hunter Stanton",
    "version": (1, 0),
    "blender": (4, 0, 2),
}

# represents a world vertex
# 3 floats, x y z
# 4 bytes, shader select 1-4 (unsure how many shaders are supported and etc.)
# 2 floats, U V
class WorldVertex:
    def __init__(self, x, y, z, r, g, b, a, u, v):
        self.x = x
        self.y = y
        self.z = z
        self.r = r
        self.g = g
        self.b = b
        self.a = a
        self.u = u
        self.v = v

# represents a texture, with mipmaps and other data
class Texture:
    def __init__(self, textureIdx, subtextureIdx, textureBank, height, width, numMipMaps, imageType, pitch, imageDataPointer, mipMap1DataPointer, mipMap2DataPointer, mipMap3DataPointer, mipMap4DataPointer, mipMap5DataPointer, mipMap6DataPointer, mipMap7DataPointer, data, ddsPath):
        self.textureIdx = textureIdx
        self.subtextureIdx = subtextureIdx
        self.bank = textureBank
        self.height = height
        self.width = width
        self.numMipMaps = numMipMaps
        self.imageType = imageType
        self.pitch = pitch
        self.imageDataPointer = imageDataPointer
        self.mipMap1DataPointer = mipMap1DataPointer
        self.mipMap2DataPointer = mipMap2DataPointer
        self.mipMap3DataPointer = mipMap3DataPointer
        self.mipMap4DataPointer = mipMap4DataPointer
        self.mipMap5DataPointer = mipMap5DataPointer
        self.mipMap6DataPointer = mipMap6DataPointer
        self.mipMap7DataPointer = mipMap7DataPointer
        self.data = data
        self.ddsPath = ddsPath

class Vector4:
    def __init__(self, x, y, z, w):
        self.x = x
        self.y = y
        self.z = z
        self.w = w

class Light:
    def __init__(self, type, red, green, blue, light_flags, intensity, scale, pos, direction, cone_angle, penumbra_angle, shadow_length):
        self.type = type
        self.red = red
        self.green = green
        self.blue = blue
        self.light_flags = light_flags
        self.intensity = intensity
        self.scale = scale
        self.pos = pos
        self.direction = direction
        self.cone_angle = cone_angle
        self.penumbra_angle = penumbra_angle
        self.shadow_length = shadow_length



class SH4WorldMeshImportOperator(bpy.types.Operator, bpy_extras.io_utils.ExportHelper):
    bl_idname = "import.sh4worldmesh"
    bl_label = "Import Silent Hill 4 World Mesh"
    bl_options = {'PRESET'}
    filename_ext = ".bin"
    
    filter_glob: bpy.props.StringProperty(
        default="*.bin",
        options={"HIDDEN"},
        maxlen=255,
    )

    def getTexturePath(self, textures: list, bankTextures: list, textureIdx: int, subtextureIdx: int, bank: int):
        """Gets the DDS file path of a texture based on its index, subtexture index, and bank number.

        Args:
            textures (list): The list of textures to search
            bankTextures (list): The list of bank textures to search
            textureIdx (int): The texture index to search for
            subtextureIdx (int): The subtexture index to search for
            bank (int): The bank number to search for

        Returns:
            string: The path to the DDS file
        """        
        self.report({"INFO"}, "Looking for texture " + str(textureIdx) + " subTexId " + str(subtextureIdx) + " bank " + str(bank))
        for texture in textures:
            if texture.textureIdx == textureIdx and texture.subtextureIdx == subtextureIdx and texture.bank == bank:
                return texture.ddsPath
        for texture in bankTextures:
            if texture.textureIdx == textureIdx and texture.subtextureIdx == subtextureIdx and texture.bank == bank:
                return texture.ddsPath
        self.report({"ERROR"}, "Texture not found")
    
    
    
    def getTexturesFromFile(self, texture_chunk_offset: int, textures: list, file, bank: int):
        """Reads the textures from a file and populates a list of textures.

        Args:
            texture_chunk_offset: The offset to the texture chunk in the file
            textures: The list of textures to populate
            file: The file to read from
            bank: The bank number to use for these textures
        """        
        self.report({"INFO"}, "Getting textures from file " + str(file) + " at offset " + str(texture_chunk_offset) + " for bank " + str(bank))
        # jump to texture chunk
        file.seek(texture_chunk_offset)
        # read texture and then palette count
        texture_count = struct.unpack("<H", file.read(2))[0]
        palette_count = struct.unpack("<H", file.read(2))[0]
        # skip 0xC bytes
        file.seek(0xC, 1)
        # read texture and palette offsets
        texture_offsets = struct.unpack("<" + "I" * (texture_count + palette_count), file.read(4 * (texture_count + palette_count)))
        # Read past the palette/texture information
        # Palette data + texture data is 0x10 in size
        file.seek(0x10 * (texture_count + palette_count), 1)
        # read texture offset
        texture_offset = file.tell()
        # jump to texture_chunk_offset + 0x10;
        file.seek(texture_chunk_offset + 0x10)
        # read data for every texture
        for i in range(texture_count):
            original_pos = file.tell()
            # jump to texture_pointer
            file.seek(texture_offsets[i] + texture_chunk_offset)
            # read texture width and height
            texture_width = struct.unpack("<I", file.read(4))[0]
            texture_height = struct.unpack("<I", file.read(4))[0]
            # jump to palette pointer
            file.seek(texture_offsets[i+texture_count] + texture_chunk_offset)
            # skip past 4 bytes
            file.seek(4, 1)
            # read number of subtextures (?)
            num_subtextures = struct.unpack("<I", file.read(4))[0]
            file.seek(4, 1)
            # read image header pointer
            imageHeaderPointer = struct.unpack("<I", file.read(4))[0]
            # jump to imageHeaderPointer
            file.seek((texture_offsets[i+texture_count] + texture_chunk_offset) + imageHeaderPointer)
            for subtexture in range(num_subtextures):
                # read 0x20 bytes
                file.seek(0x20, 1)
                # read second width/height and verify they match the first
                texture_width2 = struct.unpack("<I", file.read(4))[0]
                texture_height2 = struct.unpack("<I", file.read(4))[0]
                if texture_width != texture_width2 or texture_height != texture_height2:
                    self.report({"ERROR"}, "Texture width/height mismatch")
                # read 4 byte string that represents image type
                image_type = file.read(4).decode("utf-8")
                # read number of mip maps
                num_mip_maps = struct.unpack("<I", file.read(4))[0]
                pitch = struct.unpack("<I", file.read(4))[0]
                # read past 0x1C bytes
                file.seek(0x1C, 1)
                # read image data pointer + add texture_offset to it
                image_data_pointer = struct.unpack("<I", file.read(4))[0] + texture_offset
                # read mipmap pointer + add texture_offset to it
                mipmap_pointer1 = struct.unpack("<I", file.read(4))[0] + texture_offset
                mipmap_pointer2 = struct.unpack("<I", file.read(4))[0] + texture_offset
                mipmap_pointer3 = struct.unpack("<I", file.read(4))[0] + texture_offset
                mipmap_pointer4 = struct.unpack("<I", file.read(4))[0] + texture_offset
                mipmap_pointer5 = struct.unpack("<I", file.read(4))[0] + texture_offset
                mipmap_pointer6 = struct.unpack("<I", file.read(4))[0] + texture_offset
                mipmap_pointer7 = struct.unpack("<I", file.read(4))[0] + texture_offset
                # print height, width, height2, width2, num mip maps, and image and mip map pointers
                # create new texture object with the previous information
                new_texture = Texture(i + 1, subtexture, bank, texture_width2, texture_height2, num_mip_maps, image_type, pitch, image_data_pointer, mipmap_pointer1, mipmap_pointer2, mipmap_pointer3, mipmap_pointer4, mipmap_pointer5, mipmap_pointer6, mipmap_pointer7, None, None)
                textures.append(new_texture)
            
            file.seek(original_pos)

    def readDrawBlock(self, file, current_pos, group_offset, textures, bank_textures, group_name, collection):
        # jump to the transparency group
        file.seek(current_pos + group_offset)

        # store the current position in the file
        group_offset = file.tell()

        # read number of draw blocks
        num_draw_blocks = struct.unpack("<I", file.read(4))[0]

        # read offsets to each draw block
        draw_block_offsets = struct.unpack("<" + "I" * num_draw_blocks, file.read(num_draw_blocks * 4))

        # read draw blocks
        for index, draw_block_offset in enumerate(draw_block_offsets):
            file.seek(group_offset + draw_block_offset)

            draw_block_pos = file.tell()

            next_block_offset = struct.unpack("<I", file.read(4))[0]

            params_offset = struct.unpack("<I", file.read(4))[0]
            parts_offset = struct.unpack("<I", file.read(4))[0]

            texture_id = struct.unpack("<H", file.read(2))[0]
            texture_bank = struct.unpack("<H", file.read(2))[0]
            clut_id = struct.unpack("<B", file.read(1))[0]
            subtexture_id = struct.unpack("<B", file.read(1))[0]

            self.report({"INFO"} , "Texture ID: " + str(texture_id) + " Texture Bank: " + str(texture_bank) + " CLUT ID: " + str(clut_id) + " Subtexture ID: " + str(subtexture_id))
            palette_idx = struct.unpack("<H", file.read(2))[0]

            bounding_box_offset = struct.unpack("<I", file.read(4))[0]

            matrix_index = struct.unpack("<H", file.read(2))[0]
            backclip = struct.unpack("<B", file.read(1))[0]
            pad0 = struct.unpack("<B", file.read(1))[0]
            pad1 = struct.unpack("<I", file.read(4))[0]

            # jump to parts
            file.seek(draw_block_pos + parts_offset)

            part_pos = file.tell()

            unk = struct.unpack("<" + "B" * 0x20, file.read(0x20))
            n_faces = struct.unpack("<I", file.read(4))[0]
            n_vertices = struct.unpack("<I", file.read(4))[0]
            unk2 = struct.unpack("<" + "B" * 0x40, file.read(0x40))
            unk3 = struct.unpack("<I", file.read(4))[0]
            unk4 = struct.unpack("<I", file.read(4))[0]
            unk5 = struct.unpack("<I", file.read(4))[0]
            faces_offset = struct.unpack("<I", file.read(4))[0]
            vertices_offset = struct.unpack("<I", file.read(4))[0]
            unk6 = struct.unpack("<I", file.read(4))[0]

            # read faces as shorts
            file.seek(part_pos + faces_offset)

            faces = []
            for i in range(n_faces):
                faces.append(struct.unpack("<H", file.read(2))[0])

            vertices = []
            for i in range(n_vertices):
                # create a new WorldVertex
                vertex = WorldVertex(struct.unpack("<f", file.read(4))[0], struct.unpack("<f", file.read(4))[0], struct.unpack("<f", file.read(4))[0],
                                     struct.unpack("<" + "B", file.read(1))[0], struct.unpack("<" + "B", file.read(1))[0], struct.unpack("<" + "B", file.read(1))[0], struct.unpack("<" + "B", file.read(1))[0],
                                     struct.unpack("<f", file.read(4))[0], struct.unpack("<f", file.read(4))[0]
                                        )
                vertices.append(vertex)

            triangles = []
            for i in range(2, len(faces)):
                if i % 2 == 0:
                    triangles.append((faces[i - 2], faces[i - 1], faces[i]))
                else:
                    triangles.append((faces[i - 1], faces[i - 2], faces[i]))




            mesh_data = bpy.data.meshes.new("world_mesh")
            # create object with the index of draw group
            mesh_object = bpy.data.objects.new(group_name + "_draw_group_block_" + str(index), mesh_data)
            scale_factor = 0.05 
            mesh_object.scale = (scale_factor, scale_factor, scale_factor)

            mesh_object.rotation_euler[0] =  -math.pi / 2  # mesh needs to be rotated -90 degrees on X axis to look correct


            # Create a new material and assign it the DDS file we created earlier
            material = bpy.data.materials.new(group_name + "_material_" + str(index))
            material.use_nodes = True
            material.node_tree.nodes.clear()
            material_output = material.node_tree.nodes.new(type="ShaderNodeOutputMaterial")
            material_output.location = 400, 0
            material_output.label = "Material Output"
            principled_bsdf_node = material.node_tree.nodes.new(type="ShaderNodeBsdfPrincipled")
            principled_bsdf_node.location = 0, 0
            principled_bsdf_node.label = "Principled BSDF"
            texture_node = material.node_tree.nodes.new(type="ShaderNodeTexImage")
            texture_node.location = -200, 0
            texture_node.label = "Texture Image"
            vertex_color_node = material.node_tree.nodes.new(type="ShaderNodeVertexColor")
            vertex_color_node.location = -400, 0
            vertex_color_node.label = "Vertex Color"
            material.node_tree.links.new(principled_bsdf_node.outputs[0], material_output.inputs[0])
            material.node_tree.links.new(texture_node.outputs[0], principled_bsdf_node.inputs[0])
            material.node_tree.links.new(texture_node.outputs[1], principled_bsdf_node.inputs[4])
            material.node_tree.links.new(vertex_color_node.outputs[0], principled_bsdf_node.inputs[1])  # Connect vertex color to diffuse color
            try:
                texture_node.image = bpy.data.images.load(self.getTexturePath(textures, bank_textures, texture_id, subtexture_id, texture_bank))
            except:
                self.report({"ERROR"}, "Texture not found")
            material.blend_method = "CLIP"
            mesh_object.data.materials.append(material)


            collection.objects.link(mesh_object)


            # create a new Blender mesh using the faces and vertices without using from_pydata
            bm = bmesh.new()
    
            uv_layer = bm.loops.layers.uv.new("uvs")  # Create a UV layer
            color_layer = bm.loops.layers.color.new("color")  # Create a vertex color layer
    
            bm_verts = []
            for vertex in vertices:
                bm_vert = bm.verts.new((vertex.x, vertex.y, vertex.z))
                bm_verts.append(bm_vert)
    
            bm.verts.ensure_lookup_table()  # Update the index table
            bm.faces.ensure_lookup_table()  # Update the index table
    
            for triangle in triangles:
                vertex_indices = [index for index in triangle]
                if len(set(vertex_indices)) == 3:  # Check if all vertices are distinct
                    face = bm.faces.new([bm_verts[index] for index in vertex_indices])
    
                    # Now you can assign the UV coordinates
    
                    triIndex = 0
                    for loop in face.loops:
                        uv = (vertices[triangle[triIndex]].u, 1 - vertices[triangle[triIndex]].v)
                        loop[uv_layer].uv = uv
                        color = vertices[triangle[triIndex]].r, vertices[triangle[triIndex]].g, vertices[triangle[triIndex]].b, 255
                        loop[color_layer] = color
                        triIndex += 1
    
    
            bm.to_mesh(mesh_data)
            mesh_data.update()  # explicitly update mesh data
            bm.free()

    def execute(self, context):

        with open(self.filepath, "rb") as file:
            extra_bank_filename = os.path.basename(self.filepath)[:2] + "gb.bin"
            self.report({"INFO"}, extra_bank_filename)

            # read number of chunks inside the bin
            num_chunks = struct.unpack("<I", file.read(4))[0]

            # read offsets to each chunk in the bin, num_chunks * 4 bytes
            offsets = struct.unpack("<" + "I" * num_chunks, file.read(num_chunks * 4))

            mesh_chunk_offset = 0

            # check each offset until we find one where the first 4 bytes are 0x010003FC
            for offset in offsets:
                file.seek(offset)
                chunk_type = struct.unpack("<I", file.read(4))[0]
                if chunk_type == 0xFC030001:
                    mesh_chunk_offset = offset
                    self.report({"INFO"}, "Found mesh chunk at " + str(mesh_chunk_offset))
                    break

            slgt_chunk_offset = 0

            # check each offset until we find one where the first 4 bytes are 0x010003FC
            for offset in offsets:
                file.seek(offset)
                chunk_type = struct.unpack("<I", file.read(4))[0]
                if chunk_type == 0x54474c53:
                    slgt_chunk_offset = offset
                    self.report({"INFO"}, "Found SLGT chunk at " + str(slgt_chunk_offset))
                    break
            
            texture_chunk_offset = 0

            # check each offset until we find one where the first two shorts are equal (indicates texture chunk)
            for offset in offsets:
                file.seek(offset)
                texture_count = struct.unpack("<H", file.read(2))[0]
                palette_count = struct.unpack("<H", file.read(2))[0]
                if texture_count == palette_count:
                    texture_chunk_offset = offset
                    self.report({"INFO"}, "Found texture chunk at " + str(texture_chunk_offset))
                    break

            # if we didnt find a mesh chunk, cancel
            file.seek(mesh_chunk_offset)
            chunk_type = struct.unpack("<I", file.read(4))[0]
            if chunk_type != 0xFC030001:
                self.report({"ERROR"}, "Invalid chunk type")
                return {"CANCELLED"}
            
            # read the version of the chunk and if it isnt 3, cancel
            chunk_version = struct.unpack("<I", file.read(4))[0]
            if chunk_version != 3:
                self.report({"ERROR"}, "Invalid world mesh version")
                return {"CANCELLED"}
            
            bankTextures = []
            textures = []
            self.getTexturesFromFile(texture_chunk_offset, textures, file, 0)

            # get texture data
            for i, texture in enumerate(textures):
                file.seek(texture.imageDataPointer + (i * 0x70))
                blocks_wide = (texture.width + 3) // 4
                blocks_high = (texture.height + 3) // 4
                if texture.imageType == "DXT1":
                    block_size = 8  # DXT1 uses 8 bytes per block
                    texture.data = file.read(blocks_wide * blocks_high * block_size)
                elif texture.imageType in ["DXT3", "DXT5"]:
                    block_size = 16  # DXT3 and DXT5 use 16 bytes per block
                    texture.data = file.read(blocks_wide * blocks_high * block_size)
                else: # A8R8G8B8
                    texture.data = file.read(4 * texture.width * texture.height)
            # add DDS headers onto texture data
            for texture in textures:
                temp_dir = bpy.app.tempdir  # Get Blender's temp directory
                ddsFile = os.path.join(temp_dir, "texture_" + str(texture.bank) + "_" + str(texture.textureIdx) + "_" + str(texture.subtextureIdx) + ".dds")
                texture.ddsPath = ddsFile
                self.report({"INFO"}, "Writing texture to: " + ddsFile)
                with open(ddsFile, "wb") as dds:
                    zeros = (0,) * 44
                    zeros2 = (0,) * 20
                    zeros3 = (0,) * 16
                    dds.write(struct.pack("<I", 0x20534444) + struct.pack("<I", 0x7C) + struct.pack("<I", 0x081007) + struct.pack("<I", texture.width) + struct.pack("<I", texture.height)
                    + struct.pack("<I", texture.pitch) + struct.pack("<I", 0) + struct.pack("<I", 0) + struct.pack("<44B", *zeros) + struct.pack("<I", 0x20) + struct.pack("<I", 0x4)
                    + texture.imageType.encode('utf-8') + struct.pack("<20B", *zeros2) + struct.pack("<I", 0x1000) + struct.pack("<16B", *zeros3) + texture.data)

            try:
                with open(os.path.join(os.path.dirname(self.filepath), extra_bank_filename), "rb") as bankFile:
                    num_files = struct.unpack("<I", bankFile.read(4))[0]
                    texture_offset = struct.unpack("<I", bankFile.read(4))[0]
                    self.report({"INFO"}, "Texture offset: " + str(texture_offset))
                    self.getTexturesFromFile(texture_offset, bankTextures, bankFile, 1)

                    # get texture data
                    for i, texture in enumerate(bankTextures):
                        bankFile.seek(texture.imageDataPointer + (i * 0x70))
                        blocks_wide = (texture.width + 3) // 4
                        blocks_high = (texture.height + 3) // 4
                        if texture.imageType == "DXT1":
                            block_size = 8  # DXT1 uses 8 bytes per block
                            texture.data = bankFile.read(blocks_wide * blocks_high * block_size)
                        elif texture.imageType in ["DXT3", "DXT5"]:
                            block_size = 16  # DXT3 and DXT5 use 16 bytes per block
                            texture.data = bankFile.read(blocks_wide * blocks_high * block_size)
                        else: # A8R8G8B8
                            texture.data = bankFile.read(4 * texture.width * texture.height)
                    # add DDS headers onto texture data
                    for texture in bankTextures:
                        temp_dir = bpy.app.tempdir  # Get Blender's temp directory
                        ddsFile = os.path.join(temp_dir, "texture_" + str(texture.bank) + "_" + str(texture.textureIdx) + "_" + str(texture.subtextureIdx) + ".dds")
                        texture.ddsPath = ddsFile
                        self.report({"INFO"}, "Writing texture to: " + ddsFile)
                        with open(ddsFile, "wb") as dds:
                            zeros = (0,) * 44
                            zeros2 = (0,) * 20
                            zeros3 = (0,) * 16
                            dds.write(struct.pack("<I", 0x20534444) + struct.pack("<I", 0x7C) + struct.pack("<I", 0x081007) + struct.pack("<I", texture.width) + struct.pack("<I", texture.height)
                            + struct.pack("<I", texture.pitch) + struct.pack("<I", 0) + struct.pack("<I", 0) + struct.pack("<44B", *zeros) + struct.pack("<I", 0x20) + struct.pack("<I", 0x4)
                            + texture.imageType.encode('utf-8') + struct.pack("<20B", *zeros2) + struct.pack("<I", 0x1000) + struct.pack("<16B", *zeros3) + texture.data)
            except:
                self.report({"INFO"}, "No extra bank file found - this world mesh likely does not need an extra texture bank.")

            # jump to mesh_chunk_offset + 0x8
            file.seek(mesh_chunk_offset + 0x8)

            # store the current position in the file
            current_pos = file.tell() - 0x8


            normal_group_offset = struct.unpack("<I", file.read(4))[0]
            overdraw_group_offset = struct.unpack("<I", file.read(4))[0]
            transparency_group_offset = struct.unpack("<I", file.read(4))[0]
            matrices_offset = struct.unpack("<I", file.read(4))[0]
            n_matrices = struct.unpack("<I", file.read(4))[0]
            additional_data = struct.unpack("<I", file.read(4))[0]


            if normal_group_offset != 0:
                normal_groups_collection = bpy.data.collections.new("Normal Groups")
                bpy.context.scene.collection.children.link(normal_groups_collection)
                self.readDrawBlock(file, current_pos, normal_group_offset, textures, bankTextures, "normal", normal_groups_collection)

            if overdraw_group_offset != 0:
                overdraw_groups_collection = bpy.data.collections.new("Overdraw Groups")
                bpy.context.scene.collection.children.link(overdraw_groups_collection)
                self.readDrawBlock(file, current_pos, overdraw_group_offset, textures, bankTextures, "overdraw", overdraw_groups_collection)

            if transparency_group_offset != 0:
                transparency_groups_collection = bpy.data.collections.new("Transparency Groups")
                bpy.context.scene.collection.children.link(transparency_groups_collection)
                self.readDrawBlock(file, current_pos, transparency_group_offset, textures, bankTextures, "transparency", transparency_groups_collection)

            # TODO: figure out how to place the lights properly and how to map game brightness and etc. to Blender
            '''if slgt_chunk_offset != 0:
                lights = []

                file.seek(slgt_chunk_offset)
                # skip past header
                file.seek(0x8, 1)

                num_lights = struct.unpack("<I", file.read(4))[0]
                struct.unpack("<I", file.read(4))[0]  # padding

                for i in range(num_lights):
                    light_type = struct.unpack("<I", file.read(4))[0]

                    # red green blue are single bytes
                    light_red = struct.unpack("<B", file.read(1))[0]
                    light_green = struct.unpack("<B", file.read(1))[0]
                    light_blue = struct.unpack("<B", file.read(1))[0]
                    light_light_flags = struct.unpack("<B", file.read(1))[0]

                    light_intensity = struct.unpack("<f", file.read(4))[0]
                    light_scale = struct.unpack("<f", file.read(4))[0]

                    light_pos = Vector4(struct.unpack("<f", file.read(4))[0], struct.unpack("<f", file.read(4))[0], struct.unpack("<f", file.read(4))[0], struct.unpack("<f", file.read(4))[0])
                    light_direction = Vector4(struct.unpack("<f", file.read(4))[0], struct.unpack("<f", file.read(4))[0], struct.unpack("<f", file.read(4))[0], struct.unpack("<f", file.read(4))[0])

                    light_cone_angle = struct.unpack("<f", file.read(4))[0]
                    light_penumbra_angle = struct.unpack("<f", file.read(4))[0]
                    light_shadow_length = struct.unpack("<f", file.read(4))[0]
                    struct.unpack("<I", file.read(4))[0]  # padding

                    light = Light(light_type, light_red, light_green, light_blue, light_light_flags, light_intensity, light_scale, light_pos, light_direction, light_cone_angle, light_penumbra_angle, light_shadow_length)

                    lights.append(light)

                
                lights_collection = bpy.data.collections.new("Lights")
                bpy.context.scene.collection.children.link(lights_collection)

                # TODO: Add the rest of the light types
                for light in lights:
                    if light.type == 0:
                        self.report({"INFO"}, "Light is none type")
                        continue
                    elif light.type == 1:
                        light_data = bpy.data.lights.new(name="Ambient Light", type='SUN')
                        light_data.color = (light.red / 255, light.green / 255, light.blue / 255)
                        light_data.energy = light.scale

                        light_object = bpy.data.objects.new(name="Ambient Light", object_data=light_data)
                        # light position needs to be scaled down to 0.05
                        light_object.location = (light.pos.x * 0.05, light.pos.z * 0.05, light.pos.y * 0.05)

    
                        lights_collection.objects.link(light_object)
                    elif light.type == 2:
                        light_data = bpy.data.lights.new(name="Directional Area Light", type='AREA')
                        light_data.color = (light.red / 255, light.green / 255, light.blue / 255)
                        light_data.energy = light.scale

                        light_object = bpy.data.objects.new(name="Directional Area Light", object_data=light_data)
                        # light position needs to be scaled down to 0.05
                        light_object.location = (light.pos.x * 0.05, light.pos.z * 0.05, light.pos.y * 0.05)
                        lights_collection.objects.link(light_object)
                    elif light_type == 3:
                        light_data = bpy.data.lights.new(name="Point Light", type='POINT')
                        light_data.color = (light.red / 255, light.green / 255, light.blue / 255)
                        light_data.energy = light.scale

                        light_object = bpy.data.objects.new(name="Point Light", object_data=light_data)
                        # light position needs to be scaled down to 0.05
                        light_object.location = (light.pos.x * 0.05, light.pos.z * 0.05, light.pos.y * 0.05)
                        lights_collection.objects.link(light_object)
                    '''

        self.report({"INFO"}, "Import successful")
        return {"FINISHED"}


def menu_func_import(self, context):
    self.layout.operator(
        SH4WorldMeshImportOperator.bl_idname, text="Silent Hill 4 World Mesh (.bin)"
    )


def register():
    bpy.utils.register_class(SH4WorldMeshImportOperator)
    bpy.types.TOPBAR_MT_file_import.append(menu_func_import)


def unregister():
    bpy.utils.unregister_class(SH4WorldMeshImportOperator)
    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import)


if __name__ == "__main__":
    register()
