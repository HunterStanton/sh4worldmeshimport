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
    def __init__(
        self,
        textureIdx,
        subtextureIdx,
        textureBank,
        width,
        height,
        numMipMaps,
        imageType,
        pitch,
        imageDataPointer,
        mipMap1DataPointer,
        mipMap2DataPointer,
        mipMap3DataPointer,
        mipMap4DataPointer,
        mipMap5DataPointer,
        mipMap6DataPointer,
        mipMap7DataPointer,
        data,
        ddsPath,
    ):
        self.textureIdx = textureIdx
        self.subtextureIdx = subtextureIdx
        self.bank = textureBank
        self.width = width
        self.height = height
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
    def __init__(
        self,
        type,
        red,
        green,
        blue,
        light_flags,
        intensity,
        scale,
        pos,
        direction,
        cone_angle,
        penumbra_angle,
        shadow_length,
    ):
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


def show_message_box(message="", title="Message", icon="INFO"):

    def draw(self, context):
        layout = self.layout
        if isinstance(message, list):
            for line in message:
                layout.label(text=line)
        else:
            layout.label(text=message)

    bpy.context.window_manager.popup_menu(draw, title=title, icon=icon)


class SH4WorldMeshImportOperator(bpy.types.Operator, bpy_extras.io_utils.ImportHelper):
    bl_idname = "import.sh4worldmesh"
    bl_label = "Import Silent Hill 4 World Mesh"
    bl_options = {"PRESET"}
    filename_ext = ".bin"

    _import_warnings: list = []
    _import_errors: list = []

    filter_glob: bpy.props.StringProperty(
        default="*.bin",
        options={"HIDDEN"},
        maxlen=255,
    )

    # makes Blender recalculate normals and enable smooth shading on imported meshes
    # this is to prevent things looking very polygonal and faceted
    calculate_normals: bpy.props.BoolProperty(
        name="Calculate Normals",
        description="Recalculate vertex normals and enable smooth shading on imported meshes",
        default=True,
    )

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "calculate_normals")
        col = layout.column(align=True)
        col.scale_y = 0.85
        col.label(text="Recalculates vertex normals", icon="INFO")
        col.label(text="and enables smooth shading")
        col.label(text="to reduce faceted shading")

    def _apply_smooth_normals(self, mesh_object: bpy.types.Object):
        if not self.calculate_normals:
            return
        if mesh_object is None or mesh_object.type != "MESH":
            return
        mesh_data = mesh_object.data
        if mesh_data is None:
            return

        try:
            poly_count = len(mesh_data.polygons)
            if poly_count:
                mesh_data.polygons.foreach_set("use_smooth", [True] * poly_count)

            if hasattr(mesh_data, "use_auto_smooth"):
                mesh_data.use_auto_smooth = False

            if hasattr(mesh_data, "calc_normals"):
                mesh_data.calc_normals()
            if hasattr(mesh_data, "calc_normals_split"):
                mesh_data.calc_normals_split()

            mesh_data.update()
        except Exception as e:
            self._add_warning(
                f"Failed to calculate normals for {mesh_object.name}: {e}"
            )

    def _configure_principled_defaults(self, principled_node):
        """Set conservative, matte defaults for Principled BSDF nodes. SH4 supported phong, but not complex PBR workflows, so this helps make it look more like it would in the engine."""
        if principled_node is None:
            return

        try:
            inputs = principled_node.inputs

            if "Roughness" in inputs:
                inputs["Roughness"].default_value = 1.0

            if "Specular IOR Level" in inputs:
                inputs["Specular IOR Level"].default_value = 0.0
            if "Specular" in inputs:
                inputs["Specular"].default_value = 0.0

            if "Metallic" in inputs:
                inputs["Metallic"].default_value = 0.0
            if "Clearcoat" in inputs:
                inputs["Clearcoat"].default_value = 0.0
            if "Sheen Weight" in inputs:
                inputs["Sheen Weight"].default_value = 0.0
            if "Sheen" in inputs:
                inputs["Sheen"].default_value = 0.0
        except Exception:
            pass

    def _reset_import_state(self):
        self._import_warnings = []
        self._import_errors = []

    def _debug(self, message: str):
        if getattr(bpy.app, "debug", False):
            print(f"[sh4worldmeshimport] {message}")

    def _add_warning(self, message: str):
        self._import_warnings.append(message)

    def _add_error(self, message: str):
        self._import_errors.append(message)

    def _validate_import_filepath(self) -> bool:
        if not self.filepath:
            show_message_box("No file selected.", "Import Error", "ERROR")
            return False

        if not os.path.exists(self.filepath):
            show_message_box(
                f"File not found: {self.filepath}", "Import Error", "ERROR"
            )
            return False

        if not self.filepath.lower().endswith(".bin"):
            show_message_box(
                "Selected file is not a .bin file.", "Import Error", "ERROR"
            )
            return False

        return True

    def _open_import_file(self):
        try:
            return open(self.filepath, "rb")
        except PermissionError:
            show_message_box(
                f"Permission denied: Cannot read file.\n{self.filepath}",
                "Import Error",
                "ERROR",
            )
            return None
        except Exception as e:
            show_message_box(f"Failed to open file: {e}", "Import Error", "ERROR")
            return None

    def _read_chunk_table(self, file):
        num_chunks = struct.unpack("<I", file.read(4))[0]
        offsets = struct.unpack("<" + "I" * num_chunks, file.read(num_chunks * 4))
        return num_chunks, offsets

    def _find_all_mesh_chunk_offsets(self, file, offsets) -> list:
        """Find all mesh chunks in the file.

        Returns:
            list: List of tuples (chunk_index, offset) for each mesh chunk found
        """
        mesh_chunks = []
        for chunk_index, offset in enumerate(offsets):
            file.seek(offset)
            chunk_type = struct.unpack("<I", file.read(4))[0]
            if chunk_type == 0xFC030001:
                self.report(
                    {"INFO"},
                    f"Found mesh chunk {len(mesh_chunks)} at offset {offset} (chunk index {chunk_index})",
                )
                mesh_chunks.append((chunk_index, offset))
        return mesh_chunks

    def _find_slgt_chunk_offset(self, file, offsets) -> int:
        for offset in offsets:
            file.seek(offset)
            chunk_type = struct.unpack("<I", file.read(4))[0]
            if chunk_type == 0x54474C53:
                self.report({"INFO"}, "Found SLGT chunk at " + str(offset))
                return offset
        return 0

    def _find_texture_chunk_offset(self, file, offsets) -> int:
        for offset in offsets:
            file.seek(offset)
            texture_count = struct.unpack("<H", file.read(2))[0]
            palette_count = struct.unpack("<H", file.read(2))[0]
            if texture_count == palette_count:
                self.report({"INFO"}, "Found texture chunk at " + str(offset))
                return offset
        return 0

    def _find_all_texture_chunk_offsets(self, file, offsets) -> list:
        """Find all texture chunks in the file.

        Returns:
            list: List of offsets for each texture chunk found
        """
        texture_chunks = []
        for offset in offsets:
            file.seek(offset)
            texture_count = struct.unpack("<H", file.read(2))[0]
            palette_count = struct.unpack("<H", file.read(2))[0]
            if texture_count == palette_count and texture_count > 0:
                self.report({"INFO"}, f"Found texture chunk at {offset}")
                texture_chunks.append(offset)
        return texture_chunks

    def _validate_mesh_chunk_header(
        self, file, mesh_chunk_offset: int, chunk_index: int = 0
    ) -> tuple:
        """Validate mesh chunk header and return (is_valid, version).

        Returns:
            tuple: (is_valid: bool, version: int) - version is 0 if invalid
        """
        file.seek(mesh_chunk_offset)
        chunk_type = struct.unpack("<I", file.read(4))[0]
        if chunk_type != 0xFC030001:
            warning_msg = f"Mesh chunk {chunk_index}: Invalid chunk type 0x{chunk_type:08X} (expected 0xFC030001)"
            self.report({"WARNING"}, warning_msg)
            self._add_warning(warning_msg)
            return (False, 0)

        chunk_version = struct.unpack("<I", file.read(4))[0]
        if chunk_version not in (1, 2, 3):
            warning_msg = f"Mesh chunk {chunk_index}: Unsupported version {chunk_version} (only 1, 2, and 3 supported)"
            self.report({"WARNING"}, warning_msg)
            self._add_warning(warning_msg)
            return (False, 0)

        self.report({"INFO"}, f"Mesh chunk {chunk_index}: Version {chunk_version}")
        return (True, chunk_version)

    def _read_texture_data(self, file, textures: list):
        for i, texture in enumerate(textures):
            try:
                file.seek(texture.imageDataPointer + (i * 0x70))
                blocks_wide = (texture.width + 3) // 4
                blocks_high = (texture.height + 3) // 4
                if texture.imageType == "DXT1":
                    block_size = 8
                    texture.data = file.read(blocks_wide * blocks_high * block_size)
                elif texture.imageType in ["DXT3", "DXT5"]:
                    block_size = 16
                    texture.data = file.read(blocks_wide * blocks_high * block_size)
                else:
                    texture.data = file.read(4 * texture.width * texture.height)

                if not texture.data:
                    warning_msg = f"Texture {i} has no data (type: {texture.imageType})"
                    self.report({"WARNING"}, warning_msg)
                    self._add_warning(warning_msg)
            except Exception as e:
                warning_msg = f"Failed to read texture {i} data: {e}"
                self.report({"WARNING"}, warning_msg)
                self._add_warning(warning_msg)
                texture.data = None

    # reworked the DXT writing code, now it should be more accurate to a real DDS? doesn't really make an impact in blender, but this is more for completeness sake
    def _write_dds_files(self, textures: list):
        for texture in textures:
            if texture.data is None:
                continue

            temp_dir = bpy.app.tempdir
            dds_file = os.path.join(
                temp_dir,
                "texture_"
                + str(texture.bank)
                + "_"
                + str(texture.textureIdx)
                + "_"
                + str(texture.subtextureIdx)
                + ".dds",
            )
            texture.ddsPath = dds_file
            self.report({"INFO"}, "Writing texture to: " + dds_file)

            try:
                with open(dds_file, "wb") as dds:
                    width = int(texture.width)
                    height = int(texture.height)
                    mipmaps = int(texture.numMipMaps) if texture.numMipMaps else 0

                    # DDS magic + header
                    DDS_MAGIC = 0x20534444
                    DDS_HEADER_SIZE = 0x7C
                    DDS_PIXELFORMAT_SIZE = 0x20

                    DDSD_CAPS = 0x1
                    DDSD_HEIGHT = 0x2
                    DDSD_WIDTH = 0x4
                    DDSD_PITCH = 0x8
                    DDSD_PIXELFORMAT = 0x1000
                    DDSD_MIPMAPCOUNT = 0x20000
                    DDSD_LINEARSIZE = 0x80000

                    DDPF_ALPHAPIXELS = 0x1
                    DDPF_FOURCC = 0x4
                    DDPF_RGB = 0x40

                    DDSCAPS_COMPLEX = 0x8
                    DDSCAPS_TEXTURE = 0x1000
                    DDSCAPS_MIPMAP = 0x400000

                    img_type = (texture.imageType or "").strip("\x00")
                    is_dxt = img_type in {"DXT1", "DXT3", "DXT5"}

                    if is_dxt:
                        block_size = 8 if img_type == "DXT1" else 16
                        blocks_wide = (width + 3) // 4
                        blocks_high = (height + 3) // 4
                        linear_size = blocks_wide * blocks_high * block_size
                        pitch_or_linear = linear_size
                        header_flags = (
                            DDSD_CAPS
                            | DDSD_HEIGHT
                            | DDSD_WIDTH
                            | DDSD_PIXELFORMAT
                            | DDSD_LINEARSIZE
                        )
                        if mipmaps > 1:
                            header_flags |= DDSD_MIPMAPCOUNT

                        pf_flags = DDPF_FOURCC
                        fourcc = img_type.encode("ascii", errors="replace")[:4]
                        fourcc = fourcc.ljust(4, b"\x00")
                        rgb_bitcount = 0
                        rmask = gmask = bmask = amask = 0
                    else:
                        pitch_or_linear = width * 4
                        header_flags = (
                            DDSD_CAPS
                            | DDSD_HEIGHT
                            | DDSD_WIDTH
                            | DDSD_PIXELFORMAT
                            | DDSD_PITCH
                        )
                        if mipmaps > 1:
                            header_flags |= DDSD_MIPMAPCOUNT

                        pf_flags = DDPF_RGB | DDPF_ALPHAPIXELS
                        fourcc = b"\x00\x00\x00\x00"
                        rgb_bitcount = 32
                        rmask = 0x00FF0000
                        gmask = 0x0000FF00
                        bmask = 0x000000FF
                        amask = 0xFF000000

                    caps = DDSCAPS_TEXTURE
                    if mipmaps > 1:
                        caps |= DDSCAPS_COMPLEX | DDSCAPS_MIPMAP

                    reserved1 = (0,) * 11
                    header = struct.pack(
                        "<I"  # size
                        "I"  # flags
                        "I"  # height
                        "I"  # width
                        "I"  # pitchOrLinearSize
                        "I"  # depth
                        "I"  # mipMapCount
                        "11I",  # reserved1
                        DDS_HEADER_SIZE,
                        header_flags,
                        height,
                        width,
                        pitch_or_linear,
                        0,
                        mipmaps if mipmaps > 1 else 0,
                        *reserved1,
                    )

                    ddspf = struct.pack(
                        "<II4sIIIII",
                        DDS_PIXELFORMAT_SIZE,
                        pf_flags,
                        fourcc,
                        rgb_bitcount,
                        rmask,
                        gmask,
                        bmask,
                        amask,
                    )

                    caps_part = struct.pack("<IIIII", caps, 0, 0, 0, 0)

                    dds.write(struct.pack("<I", DDS_MAGIC))
                    dds.write(header)
                    dds.write(ddspf)
                    dds.write(caps_part)
                    dds.write(texture.data)
            except Exception as e:
                warning_msg = (
                    f"Failed to write DDS file for texture {texture.textureIdx}: {e}"
                )
                self.report({"WARNING"}, warning_msg)
                self._add_warning(warning_msg)
                texture.ddsPath = None

    def _try_load_bank_textures(self, extra_bank_filename: str):
        bank_textures = []
        try:
            with open(
                os.path.join(os.path.dirname(self.filepath), extra_bank_filename),
                "rb",
            ) as bank_file:
                num_files = struct.unpack("<I", bank_file.read(4))[0]
                texture_offset = struct.unpack("<I", bank_file.read(4))[0]
                self.report({"INFO"}, "Texture offset: " + str(texture_offset))
                self.getTexturesFromFile(texture_offset, bank_textures, bank_file, 1)

                for i, texture in enumerate(bank_textures):
                    bank_file.seek(texture.imageDataPointer + (i * 0x70))
                    blocks_wide = (texture.width + 3) // 4
                    blocks_high = (texture.height + 3) // 4
                    if texture.imageType == "DXT1":
                        block_size = 8
                        texture.data = bank_file.read(
                            blocks_wide * blocks_high * block_size
                        )
                    elif texture.imageType in ["DXT3", "DXT5"]:
                        block_size = 16
                        texture.data = bank_file.read(
                            blocks_wide * blocks_high * block_size
                        )
                    else:  # A8R8G8B8
                        texture.data = bank_file.read(
                            4 * texture.width * texture.height
                        )

                self._write_dds_files(bank_textures)
        except FileNotFoundError:
            self.report(
                {"INFO"},
                "No extra bank file found - this world mesh likely does not need an extra texture bank.",
            )
        except Exception as e:
            self.report({"WARNING"}, f"Error reading bank file: {e}")

        return bank_textures

    def _import_mesh_groups(
        self,
        file,
        mesh_chunk_offset: int,
        textures: list,
        bank_textures: list,
        mesh_index: int = 0,
        chunk_index: int = 0,
        parent_collection=None,
        chunk_version: int = 2,
    ):
        # version 1 uses pattern-based mesh finding, as opposed to proper reading
        # luckily there are only two in all of SH4, so this is reliable
        if chunk_version == 1:
            self._import_mesh_groups_v1(
                file,
                mesh_chunk_offset,
                textures,
                bank_textures,
                mesh_index,
                chunk_index,
                parent_collection,
            )
            return

        file.seek(mesh_chunk_offset + 0x8)
        current_pos = file.tell() - 0x8

        normal_group_offset = struct.unpack("<I", file.read(4))[0]
        overdraw_group_offset = struct.unpack("<I", file.read(4))[0]
        transparency_group_offset = struct.unpack("<I", file.read(4))[0]
        matrices_offset = struct.unpack("<I", file.read(4))[0]
        n_matrices = struct.unpack("<I", file.read(4))[0]
        additional_data = struct.unpack("<I", file.read(4))[0]

        mesh_prefix = f"Mesh{mesh_index:02d}_Chunk{chunk_index:02d}"

        if normal_group_offset != 0:
            normal_groups_collection = bpy.data.collections.new(f"{mesh_prefix}_Normal")
            parent_collection.children.link(normal_groups_collection)
            self.readDrawBlock(
                file,
                current_pos,
                normal_group_offset,
                textures,
                bank_textures,
                f"{mesh_prefix}_normal",
                normal_groups_collection,
            )

        if overdraw_group_offset != 0:
            overdraw_groups_collection = bpy.data.collections.new(
                f"{mesh_prefix}_Overdraw"
            )
            parent_collection.children.link(overdraw_groups_collection)
            self.readDrawBlock(
                file,
                current_pos,
                overdraw_group_offset,
                textures,
                bank_textures,
                f"{mesh_prefix}_overdraw",
                overdraw_groups_collection,
            )

        if transparency_group_offset != 0:
            transparency_groups_collection = bpy.data.collections.new(
                f"{mesh_prefix}_Transparency"
            )
            parent_collection.children.link(transparency_groups_collection)
            self.readDrawBlock(
                file,
                current_pos,
                transparency_group_offset,
                textures,
                bank_textures,
                f"{mesh_prefix}_transparency",
                transparency_groups_collection,
            )

    def _find_v1_mesh_patterns(self, data: bytes) -> list:
        # pattern to indicate the start of v1 mesh data
        pattern = b"\x03\x01\x00\x01\x01\x80"
        results = []
        i = data.find(pattern)
        mesh_count = 0

        while i != -1:
            mesh_offset = i + 6

            texture_id = 1  # Default
            subtexture_id = 0
            texture_bank = 0

            if i >= 8:
                tex_id_m4 = struct.unpack_from("<H", data, i - 4)[0]
                bank_m2 = struct.unpack_from("<H", data, i - 2)[0]
                texture_id = tex_id_m4
                texture_bank = bank_m2
                if texture_id == 0 or texture_id > 100 or texture_bank > 10:
                    texture_id = 1
                    texture_bank = 0

            results.append(
                (mesh_offset, texture_id, subtexture_id, texture_bank, mesh_count)
            )
            mesh_count += 1
            i = data.find(pattern, i + 1)
        return results

    def _create_v1_triangle_list(self, skip_list: list) -> list:
        triangles = []
        num = len(skip_list)
        start_dir = -1
        face_dir = start_dir
        f1 = 0
        f2 = 1

        for i in range(num):
            f3 = i
            skip_flag = skip_list[i]  # skip isolated vertex
            face_dir *= -1

            if not skip_flag:
                if f1 != f2 and f2 != f3 and f3 != f1:
                    if face_dir > 0:
                        triangles.append((f1, f2, f3))
                    else:
                        triangles.append((f2, f1, f3))
            else:
                face_dir = start_dir

            f1 = f2
            f2 = f3

        return triangles

    def _find_v1_draw_groups(self, chunk_data: bytes, chunk_base: int) -> list:
        draw_blocks = []

        # read header offsets (same as V2/V3)
        normal_offset = struct.unpack_from("<I", chunk_data, 0x08)[0]
        overdraw_offset = struct.unpack_from("<I", chunk_data, 0x0C)[0]
        transparency_offset = struct.unpack_from("<I", chunk_data, 0x10)[0]

        self._debug(
            f"V1 Header: normal=0x{normal_offset:X}, overdraw=0x{overdraw_offset:X}, transparency=0x{transparency_offset:X}"
        )

        group_offsets = [
            ("normal", normal_offset),
            ("overdraw", overdraw_offset),
            ("transparency", transparency_offset),
        ]

        for group_name, group_offset in group_offsets:
            if group_offset == 0:
                continue

            # r ead count and offsets for this group
            count = struct.unpack_from("<I", chunk_data, group_offset)[0]
            self._debug(
                f"{group_name} group at 0x{group_offset:X}: {count} draw blocks"
            )

            if count == 0 or count > 10000:
                continue

            # read the offset array
            offsets_start = group_offset + 4
            for i in range(count):
                block_rel_offset = struct.unpack_from(
                    "<I", chunk_data, offsets_start + i * 4
                )[0]
                # offset is relative to the start of the group (where count is)
                block_offset = group_offset + block_rel_offset

                # read draw block header
                next_block = struct.unpack_from("<I", chunk_data, block_offset + 0x00)[
                    0
                ]
                params_offset = struct.unpack_from(
                    "<I", chunk_data, block_offset + 0x04
                )[0]
                parts_offset = struct.unpack_from(
                    "<I", chunk_data, block_offset + 0x08
                )[0]
                tex_id = struct.unpack_from("<H", chunk_data, block_offset + 0x0C)[0]
                tex_bank = struct.unpack_from("<H", chunk_data, block_offset + 0x0E)[0]
                clut_id = chunk_data[block_offset + 0x10]
                subtex_id = chunk_data[block_offset + 0x11]

                # calculate where this block ends (for finding mesh patterns within it)
                # next_block is relative to block_offset
                block_end = (
                    block_offset + next_block if next_block > 0 else len(chunk_data)
                )

                self._debug(
                    f"Block {i} at 0x{block_offset:X}: tex={tex_id}, bank={tex_bank}, subtex={subtex_id}, parts@0x{parts_offset:X}, next=0x{next_block:X}, end@0x{block_end:X}"
                )

                draw_blocks.append(
                    {
                        "group": group_name,
                        "offset": block_offset,
                        "parts_offset": block_offset + parts_offset,
                        "end_offset": block_end,
                        "texture_id": tex_id,
                        "texture_bank": tex_bank,
                        "subtexture_id": subtex_id,
                    }
                )

        return draw_blocks

    def _import_mesh_groups_v1(
        self,
        file,
        mesh_chunk_offset: int,
        textures: list,
        bank_textures: list,
        mesh_index: int,
        chunk_index: int,
        parent_collection,
    ):
        # Read the entire chunk data
        file.seek(mesh_chunk_offset)
        start_pos = file.tell()
        file.seek(0, 2)
        end_pos = file.tell()
        file.seek(start_pos)

        chunk_size = min(end_pos - start_pos, 10 * 1024 * 1024)
        chunk_data = file.read(chunk_size)

        self._debug(
            f"Main textures: {[(t.textureIdx, t.subtextureIdx, t.bank) for t in textures]}"
        )
        self._debug(
            f"Bank textures: {[(t.textureIdx, t.subtextureIdx, t.bank) for t in bank_textures]}"
        )

        draw_blocks = self._find_v1_draw_groups(chunk_data, mesh_chunk_offset)

        if not draw_blocks:
            self.report({"WARNING"}, f"No V1 draw blocks found in chunk {chunk_index}")
            return

        self.report(
            {"INFO"},
            f"V1 import: {len(textures)} main + {len(bank_textures)} bank textures, {len(draw_blocks)} draw blocks",
        )

        mesh_prefix = f"Mesh{mesh_index:02d}_Chunk{chunk_index:02d}_V1"

        collections = {}
        for group_name in ["normal", "overdraw", "transparency"]:
            if any(b["group"] == group_name for b in draw_blocks):
                coll = bpy.data.collections.new(f"{mesh_prefix}_{group_name}")
                parent_collection.children.link(coll)
                collections[group_name] = coll

        texture_usage = {}
        mesh_pattern = b"\x03\x01\x00\x01\x01\x80"

        for block_idx, block in enumerate(draw_blocks):
            tex_id = block["texture_id"]
            tex_bank = block["texture_bank"]
            subtex_id = block["subtexture_id"]
            group_name = block["group"]
            collection = collections.get(group_name, parent_collection)

            key = (tex_id, subtex_id, tex_bank)
            if key not in texture_usage:
                texture_usage[key] = 0

            block_start = block["parts_offset"]
            block_end = block["end_offset"]

            all_vertices = []
            all_uvs = []
            all_colors = []
            all_triangles = []
            vertex_offset = 0

            pos = block_start
            submesh_count = 0
            while pos < block_end:
                pattern_pos = chunk_data.find(mesh_pattern, pos, block_end)
                if pattern_pos == -1:
                    break

                mesh_offset = pattern_pos + 6
                texture_usage[key] += 1

                try:
                    submesh_data = self._parse_v1_submesh(chunk_data, mesh_offset)
                    if submesh_data:
                        vertices, uvs, colors, triangles = submesh_data
                        # add vertices, uvs, colors directly
                        all_vertices.extend(vertices)
                        all_uvs.extend(uvs)
                        all_colors.extend(colors)
                        for tri in triangles:
                            all_triangles.append(
                                (
                                    tri[0] + vertex_offset,
                                    tri[1] + vertex_offset,
                                    tri[2] + vertex_offset,
                                )
                            )
                        vertex_offset += len(vertices)
                        submesh_count += 1
                except Exception as e:
                    warning_msg = (
                        f"Failed to parse v1 sub-mesh {block_idx}/{submesh_count}: {e}"
                    )
                    self.report({"WARNING"}, warning_msg)
                    self._add_warning(warning_msg)

                pos = pattern_pos + 1

            # create single combined mesh for this draw block
            # if we do not do this, there are hundreds of small meshes in blender which cause a lot of lag, combining them together is a huge
            # speedup
            if all_vertices and all_triangles:
                mesh_name = f"{mesh_prefix}_{group_name}_block{block_idx:03d}"
                try:
                    self._create_v1_mesh_object(
                        all_vertices,
                        all_uvs,
                        all_colors,
                        all_triangles,
                        textures,
                        bank_textures,
                        mesh_name,
                        collection,
                        tex_id,
                        subtex_id,
                        tex_bank,
                    )
                    self.report(
                        {"INFO"},
                        f"Created {mesh_name}: {len(all_vertices)} verts, {len(all_triangles)} tris from {submesh_count} submeshes",
                    )
                except Exception as e:
                    warning_msg = f"Failed to create mesh for block {block_idx}: {e}"
                    self.report({"WARNING"}, warning_msg)
                    self._add_warning(warning_msg)

        self._debug(f"Final texture usage: {texture_usage}")

    def _parse_v1_submesh(self, data: bytes, offset: int):
        pos = offset

        # Read vertex count
        num_vertices = data[pos]
        pos += 1

        if num_vertices == 0:
            return None

        # Skip 1 byte
        pos += 1

        # read vertices: 12 bytes position + 2 bytes flag + 2 bytes padding = 16 bytes each
        # got the information about how to read this from https://github.com/Sparagas/Silent-Hill/blob/main/Noesis%20-%20Python%20Plugins/fmt_sh4_map_bin_ps2.py
        vertices = []
        skip_list = []

        for i in range(num_vertices):
            x = struct.unpack_from("<f", data, pos)[0]
            y = struct.unpack_from("<f", data, pos + 4)[0]
            z = struct.unpack_from("<f", data, pos + 8)[0]
            pos += 12

            flag = struct.unpack_from("<H", data, pos)[0]
            skip_flag = (flag & 0x8000) == 0x8000
            pos += 2
            pos += 2  # padding

            vertices.append({"x": x, "y": y, "z": z})
            skip_list.append(skip_flag)

        pos += 8

        vertex_colors = []
        for i in range(num_vertices):
            r = data[pos]
            g = data[pos + 1]
            b = data[pos + 2]
            a = data[pos + 3]
            pos += 4
            vertex_colors.append({"r": r, "g": g, "b": b, "a": a})

        pos += 8

        uvs = []
        for i in range(num_vertices):
            u = struct.unpack_from("<f", data, pos)[0]
            v = struct.unpack_from("<f", data, pos + 4)[0]
            pos += 8
            uvs.append({"u": u, "v": v})

        triangles = self._create_v1_triangle_list(skip_list)

        if not triangles:
            return None

        return (vertices, uvs, vertex_colors, triangles)

    def _create_v1_mesh_object(
        self,
        vertices: list,
        uvs: list,
        vertex_colors: list,
        triangles: list,
        textures: list,
        bank_textures: list,
        mesh_name: str,
        collection,
        texture_id: int,
        subtexture_id: int,
        texture_bank: int,
    ):
        mesh_data = bpy.data.meshes.new(mesh_name)
        mesh_object = bpy.data.objects.new(mesh_name, mesh_data)

        scale_factor = 0.05
        mesh_object.scale = (scale_factor, scale_factor, scale_factor)
        mesh_object.rotation_euler[0] = -math.pi / 2

        # Create material
        material = bpy.data.materials.new(mesh_name + "_material")
        material.use_nodes = True
        material.node_tree.nodes.clear()

        material_output = material.node_tree.nodes.new(type="ShaderNodeOutputMaterial")
        material_output.location = 400, 0

        principled_bsdf_node = material.node_tree.nodes.new(
            type="ShaderNodeBsdfPrincipled"
        )
        principled_bsdf_node.location = 0, 0

        self._configure_principled_defaults(principled_bsdf_node)

        texture_node = material.node_tree.nodes.new(type="ShaderNodeTexImage")
        texture_node.location = -200, 0

        vertex_color_node = material.node_tree.nodes.new(type="ShaderNodeVertexColor")
        vertex_color_node.location = -400, 0

        material.node_tree.links.new(
            principled_bsdf_node.outputs[0], material_output.inputs[0]
        )
        material.node_tree.links.new(
            texture_node.outputs[0], principled_bsdf_node.inputs[0]
        )
        material.node_tree.links.new(
            texture_node.outputs[1], principled_bsdf_node.inputs[4]
        )
        material.node_tree.links.new(
            vertex_color_node.outputs[0], principled_bsdf_node.inputs[1]
        )

        # Load texture
        texture_path = self.getTexturePath(
            textures, bank_textures, texture_id, subtexture_id, texture_bank
        )
        if texture_path is not None:
            try:
                texture_node.image = bpy.data.images.load(texture_path)
            except Exception:
                pass

        material.blend_method = "CLIP"
        mesh_object.data.materials.append(material)
        collection.objects.link(mesh_object)

        # build mesh with bmesh
        bm = bmesh.new()
        try:
            uv_layer = bm.loops.layers.uv.new("uvs")
            color_layer = bm.loops.layers.color.new("color")

            bm_verts = []
            for v in vertices:
                bm_vert = bm.verts.new((v["x"], v["y"], v["z"]))
                bm_verts.append(bm_vert)

            bm.verts.ensure_lookup_table()

            for tri in triangles:
                v0, v1, v2 = tri
                if v0 < len(bm_verts) and v1 < len(bm_verts) and v2 < len(bm_verts):
                    try:
                        face = bm.faces.new([bm_verts[v0], bm_verts[v1], bm_verts[v2]])
                        for loop_idx, loop in enumerate(face.loops):
                            vert_idx = tri[loop_idx]
                            if vert_idx < len(uvs):
                                loop[uv_layer].uv = (
                                    uvs[vert_idx]["u"],
                                    1.0 - uvs[vert_idx]["v"],
                                )
                            if vert_idx < len(vertex_colors):
                                vc = vertex_colors[vert_idx]
                                loop[color_layer] = (
                                    vc["r"] / 255.0,
                                    vc["g"] / 255.0,
                                    vc["b"] / 255.0,
                                    1.0,
                                )
                    except ValueError:
                        pass

            bm.normal_update()
            bm.to_mesh(mesh_data)
            mesh_data.update()
        finally:
            bm.free()

        self._apply_smooth_normals(mesh_object)

    def _import_lights_from_slgt(self, file, slgt_chunk_offset: int):
        if slgt_chunk_offset == 0:
            return

        lights = []

        file.seek(slgt_chunk_offset)
        file.seek(0x8, 1)  # skip past header (magic + version)

        num_lights = struct.unpack("<I", file.read(4))[0]
        struct.unpack("<I", file.read(4))[0]  # padding

        self.report({"INFO"}, f"Found {num_lights} lights in SLGT chunk")

        for i in range(num_lights):
            light_type = struct.unpack("<I", file.read(4))[0]

            light_red = struct.unpack("<B", file.read(1))[0]
            light_green = struct.unpack("<B", file.read(1))[0]
            light_blue = struct.unpack("<B", file.read(1))[0]
            light_light_flags = struct.unpack("<B", file.read(1))[0]

            light_intensity = struct.unpack("<f", file.read(4))[0]
            light_scale = struct.unpack("<f", file.read(4))[0]

            light_pos = Vector4(
                struct.unpack("<f", file.read(4))[0],
                struct.unpack("<f", file.read(4))[0],
                struct.unpack("<f", file.read(4))[0],
                struct.unpack("<f", file.read(4))[0],
            )
            light_direction = Vector4(
                struct.unpack("<f", file.read(4))[0],
                struct.unpack("<f", file.read(4))[0],
                struct.unpack("<f", file.read(4))[0],
                struct.unpack("<f", file.read(4))[0],
            )

            light_cone_angle = struct.unpack("<f", file.read(4))[0]
            light_penumbra_angle = struct.unpack("<f", file.read(4))[0]
            light_shadow_length = struct.unpack("<f", file.read(4))[0]
            struct.unpack("<I", file.read(4))[0]  # padding

            light = Light(
                light_type,
                light_red,
                light_green,
                light_blue,
                light_light_flags,
                light_intensity,
                light_scale,
                light_pos,
                light_direction,
                light_cone_angle,
                light_penumbra_angle,
                light_shadow_length,
            )
            lights.append(light)

            self.report(
                {"INFO"},
                f"Light {i}: type={light_type}, pos=({light_pos.x:.2f}, {light_pos.y:.2f}, {light_pos.z:.2f}), intensity={light_intensity}, scale={light_scale}",
            )

        lights_collection = bpy.data.collections.new("Lights")
        bpy.context.scene.collection.children.link(lights_collection)

        scale_factor = 0.05  # Same scale as meshes

        for idx, light in enumerate(lights):
            light_name = f"Light_{idx}"
            light_data = None

            color = (light.red / 255.0, light.green / 255.0, light.blue / 255.0)

            pos_x = light.pos.x * scale_factor
            pos_y = light.pos.z * scale_factor
            pos_z = -light.pos.y * scale_factor

            energy = light.scale * 0.1
            if energy <= 0:
                energy = 1.0

            if light.type == 0:
                self.report({"INFO"}, f"Light {idx}: Disabled (type 0)")
                continue
            elif light.type == 1:
                light_name = f"Ambient_Light_{idx}"
                light_data = bpy.data.lights.new(name=light_name, type="SUN")
                light_data.color = color
                light_data.energy = energy * 0.1
            elif light.type == 2:
                light_name = f"Area_Light_{idx}"
                light_data = bpy.data.lights.new(name=light_name, type="AREA")
                light_data.color = color
                light_data.energy = energy * 100.0
                light_data.size = 1.0
            elif light.type == 3:
                light_name = f"Point_Light_{idx}"
                light_data = bpy.data.lights.new(name=light_name, type="POINT")
                light_data.color = color
                light_data.energy = energy * 100.0
            elif light.type == 4 or light.cone_angle > 0:
                light_name = f"Spot_Light_{idx}"
                light_data = bpy.data.lights.new(name=light_name, type="SPOT")
                light_data.color = color
                light_data.energy = energy * 100.0
                if light.cone_angle > 0:
                    light_data.spot_size = math.radians(light.cone_angle * 2)
                if light.penumbra_angle > 0:
                    light_data.spot_blend = (
                        min(light.penumbra_angle / light.cone_angle, 1.0)
                        if light.cone_angle > 0
                        else 0.5
                    )
            else:
                light_name = f"Unknown_Light_{idx}_type{light.type}"
                light_data = bpy.data.lights.new(name=light_name, type="POINT")
                light_data.color = color
                light_data.energy = energy * 100.0
                self._add_warning(f"Unknown light type {light.type} at index {idx}")

            if light_data:
                light_object = bpy.data.objects.new(
                    name=light_name, object_data=light_data
                )
                light_object.location = (pos_x, pos_y, pos_z)
                lights_collection.objects.link(light_object)
                self.report(
                    {"INFO"},
                    f"Created {light_name} at ({pos_x:.2f}, {pos_y:.2f}, {pos_z:.2f})",
                )

    def _show_import_summary(self):
        if self._import_errors:
            error_messages = [
                "Import completed with errors:",
                "",
            ] + self._import_errors[:5]
            if len(self._import_errors) > 5:
                error_messages.append(
                    f"...and {len(self._import_errors) - 5} more errors."
                )
            show_message_box(error_messages, "Import Errors", "ERROR")
        elif self._import_warnings:
            warning_messages = [
                "Import completed with warnings:",
                "",
            ] + self._import_warnings[:5]
            if len(self._import_warnings) > 5:
                warning_messages.append(
                    f"...and {len(self._import_warnings) - 5} more warnings."
                )
            show_message_box(warning_messages, "Import Warnings", "WARNING")
        else:
            show_message_box(
                "World mesh imported successfully!", "Import Complete", "INFO"
            )

    def getTexturePath(
        self,
        textures: list,
        bankTextures: list,
        textureIdx: int,
        subtextureIdx: int,
        bank: int,
    ):
        # This function is called extremely often; keep it quiet unless failing.
        for texture in textures:
            if (
                texture.textureIdx == textureIdx
                and texture.subtextureIdx == subtextureIdx
                and texture.bank == bank
            ):
                return texture.ddsPath
        for texture in bankTextures:
            if (
                texture.textureIdx == textureIdx
                and texture.subtextureIdx == subtextureIdx
                and texture.bank == bank
            ):
                return texture.ddsPath
        self.report(
            {"ERROR"},
            f"Texture not found: idx={textureIdx}, subIdx={subtextureIdx}, bank={bank}",
        )
        return None

    def getTexturesFromFile(
        self, texture_chunk_offset: int, textures: list, file, bank: int
    ):
        self.report(
            {"INFO"},
            "Getting textures from file "
            + str(file)
            + " at offset "
            + str(texture_chunk_offset)
            + " for bank "
            + str(bank),
        )

        try:
            # Validate offset
            if texture_chunk_offset < 0:
                self._import_errors.append(
                    f"Invalid texture chunk offset: {texture_chunk_offset}"
                )
                return False

            # jump to texture chunk
            file.seek(texture_chunk_offset)
            # read texture and then palette count
            texture_count = struct.unpack("<H", file.read(2))[0]
            palette_count = struct.unpack("<H", file.read(2))[0]
            # skip 0xC bytes
            file.seek(0xC, 1)
            # read texture and palette offsets
            texture_offsets = struct.unpack(
                "<" + "I" * (texture_count + palette_count),
                file.read(4 * (texture_count + palette_count)),
            )
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
                file.seek(texture_offsets[i + texture_count] + texture_chunk_offset)
                # skip past 4 bytes
                file.seek(4, 1)
                # read number of subtextures (?)
                num_subtextures = struct.unpack("<I", file.read(4))[0]
                file.seek(4, 1)
                # read image header pointer
                imageHeaderPointer = struct.unpack("<I", file.read(4))[0]
                # jump to imageHeaderPointer
                file.seek(
                    (texture_offsets[i + texture_count] + texture_chunk_offset)
                    + imageHeaderPointer
                )
                for subtexture in range(num_subtextures):
                    # read 0x20 bytes
                    file.seek(0x20, 1)
                    # read second width/height and verify they match the first
                    # if they don't, something is probably wrong
                    texture_width2 = struct.unpack("<I", file.read(4))[0]
                    texture_height2 = struct.unpack("<I", file.read(4))[0]
                    if (
                        texture_width != texture_width2
                        or texture_height != texture_height2
                    ):
                        warning_msg = f"Texture {i} subtexture {subtexture}: width/height mismatch ({texture_width}x{texture_height} vs {texture_width2}x{texture_height2})"
                        self.report({"WARNING"}, warning_msg)
                        self._import_warnings.append(warning_msg)
                    # read 4 byte string that represents image type
                    # this is pretty reliable for determining format
                    image_type = file.read(4).decode("utf-8", errors="replace")
                    # read number of mip maps
                    num_mip_maps = struct.unpack("<I", file.read(4))[0]
                    pitch = struct.unpack("<I", file.read(4))[0]
                    # skip past 0x1C bytes
                    file.seek(0x1C, 1)
                    # read image data pointer
                    image_data_pointer = (
                        struct.unpack("<I", file.read(4))[0] + texture_offset
                    )
                    # read mipmap pointers
                    mipmap_pointer1 = (
                        struct.unpack("<I", file.read(4))[0] + texture_offset
                    )
                    mipmap_pointer2 = (
                        struct.unpack("<I", file.read(4))[0] + texture_offset
                    )
                    mipmap_pointer3 = (
                        struct.unpack("<I", file.read(4))[0] + texture_offset
                    )
                    mipmap_pointer4 = (
                        struct.unpack("<I", file.read(4))[0] + texture_offset
                    )
                    mipmap_pointer5 = (
                        struct.unpack("<I", file.read(4))[0] + texture_offset
                    )
                    mipmap_pointer6 = (
                        struct.unpack("<I", file.read(4))[0] + texture_offset
                    )
                    mipmap_pointer7 = (
                        struct.unpack("<I", file.read(4))[0] + texture_offset
                    )
                    new_texture = Texture(
                        i + 1,
                        subtexture,
                        bank,
                        texture_width2,
                        texture_height2,
                        num_mip_maps,
                        image_type,
                        pitch,
                        image_data_pointer,
                        mipmap_pointer1,
                        mipmap_pointer2,
                        mipmap_pointer3,
                        mipmap_pointer4,
                        mipmap_pointer5,
                        mipmap_pointer6,
                        mipmap_pointer7,
                        None,
                        None,
                    )
                    textures.append(new_texture)

                file.seek(original_pos)

            return True

        except struct.error as e:
            error_msg = (
                f"Failed to read texture data (bank {bank}): Invalid data format - {e}"
            )
            self.report({"ERROR"}, error_msg)
            self._import_errors.append(error_msg)
            return False
        except Exception as e:
            error_msg = f"Failed to read textures (bank {bank}): {e}"
            self.report({"ERROR"}, error_msg)
            self._import_errors.append(error_msg)
            return False

    def readDrawBlock(
        self,
        file,
        current_pos,
        group_offset,
        textures,
        bank_textures,
        group_name,
        collection,
    ):
        """Reads a draw block from the file and creates mesh objects.

        Returns:
            bool: True if successful, False if an error occurred
        """
        try:
            # jump to the transparency group
            file.seek(current_pos + group_offset)

            # store the current position in the file
            group_base_offset = file.tell()

            # read number of draw blocks
            num_draw_blocks = struct.unpack("<I", file.read(4))[0]

            if num_draw_blocks == 0:
                self.report({"INFO"}, f"No draw blocks found in {group_name} group")
                return True

            if num_draw_blocks > 10000:  # Sanity check
                error_msg = (
                    f"Invalid number of draw blocks in {group_name}: {num_draw_blocks}"
                )
                self.report({"ERROR"}, error_msg)
                self._import_errors.append(error_msg)
                return False

            # read offsets to each draw block
            draw_block_offsets = struct.unpack(
                "<" + "I" * num_draw_blocks, file.read(num_draw_blocks * 4)
            )

            # read draw blocks
            for index, draw_block_offset in enumerate(draw_block_offsets):
                file.seek(group_base_offset + draw_block_offset)

                draw_block_pos = file.tell()

                next_block_offset = struct.unpack("<I", file.read(4))[0]

                params_offset = struct.unpack("<I", file.read(4))[0]
                parts_offset = struct.unpack("<I", file.read(4))[0]

                texture_id = struct.unpack("<H", file.read(2))[0]
                texture_bank = struct.unpack("<H", file.read(2))[0]
                clut_id = struct.unpack("<B", file.read(1))[0]
                subtexture_id = struct.unpack("<B", file.read(1))[0]

                self.report(
                    {"INFO"},
                    "Texture ID: "
                    + str(texture_id)
                    + " Texture Bank: "
                    + str(texture_bank)
                    + " CLUT ID: "
                    + str(clut_id)
                    + " Subtexture ID: "
                    + str(subtexture_id),
                )
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
                    vertex = WorldVertex(
                        struct.unpack("<f", file.read(4))[0],
                        struct.unpack("<f", file.read(4))[0],
                        struct.unpack("<f", file.read(4))[0],
                        struct.unpack("<" + "B", file.read(1))[0],
                        struct.unpack("<" + "B", file.read(1))[0],
                        struct.unpack("<" + "B", file.read(1))[0],
                        struct.unpack("<" + "B", file.read(1))[0],
                        struct.unpack("<f", file.read(4))[0],
                        struct.unpack("<f", file.read(4))[0],
                    )
                    vertices.append(vertex)

                triangles = []
                invalid_faces = 0
                for i in range(2, len(faces)):
                    # Validate face indices are within bounds
                    face_indices = (
                        [faces[i - 2], faces[i - 1], faces[i]]
                        if i % 2 == 0
                        else [faces[i - 1], faces[i - 2], faces[i]]
                    )
                    if all(idx < n_vertices for idx in face_indices):
                        if i % 2 == 0:
                            triangles.append((faces[i - 2], faces[i - 1], faces[i]))
                        else:
                            triangles.append((faces[i - 1], faces[i - 2], faces[i]))
                    else:
                        invalid_faces += 1

                if invalid_faces > 0:
                    warning_msg = f"{group_name} block {index}: Skipped {invalid_faces} faces with out-of-bounds vertex indices"
                    self.report({"WARNING"}, warning_msg)
                    self._import_warnings.append(warning_msg)

                mesh_data = bpy.data.meshes.new("world_mesh")
                # create object with the index of draw group
                mesh_object = bpy.data.objects.new(
                    group_name + "_draw_group_block_" + str(index), mesh_data
                )
                scale_factor = 0.05
                mesh_object.scale = (scale_factor, scale_factor, scale_factor)

                mesh_object.rotation_euler[0] = (
                    -math.pi / 2
                )  # mesh needs to be rotated -90 degrees on X axis to look correct

                # Create a new material and assign it the DDS file we created earlier
                material = bpy.data.materials.new(
                    group_name + "_material_" + str(index)
                )
                material.use_nodes = True
                material.node_tree.nodes.clear()
                material_output = material.node_tree.nodes.new(
                    type="ShaderNodeOutputMaterial"
                )
                material_output.location = 400, 0
                material_output.label = "Material Output"
                principled_bsdf_node = material.node_tree.nodes.new(
                    type="ShaderNodeBsdfPrincipled"
                )
                principled_bsdf_node.location = 0, 0
                principled_bsdf_node.label = "Principled BSDF"

                self._configure_principled_defaults(principled_bsdf_node)
                texture_node = material.node_tree.nodes.new(type="ShaderNodeTexImage")
                texture_node.location = -200, 0
                texture_node.label = "Texture Image"
                vertex_color_node = material.node_tree.nodes.new(
                    type="ShaderNodeVertexColor"
                )
                vertex_color_node.location = -400, 0
                vertex_color_node.label = "Vertex Color"
                material.node_tree.links.new(
                    principled_bsdf_node.outputs[0], material_output.inputs[0]
                )
                material.node_tree.links.new(
                    texture_node.outputs[0], principled_bsdf_node.inputs[0]
                )
                material.node_tree.links.new(
                    texture_node.outputs[1], principled_bsdf_node.inputs[4]
                )
                material.node_tree.links.new(
                    vertex_color_node.outputs[0], principled_bsdf_node.inputs[1]
                )  # Connect vertex color to diffuse color
                texture_path = self.getTexturePath(
                    textures, bank_textures, texture_id, subtexture_id, texture_bank
                )
                if texture_path is not None:
                    try:
                        texture_node.image = bpy.data.images.load(texture_path)
                    except Exception as e:
                        self.report({"ERROR"}, f"Failed to load texture: {e}")
                material.blend_method = "CLIP"
                mesh_object.data.materials.append(material)

                collection.objects.link(mesh_object)

                # create a new mesh using the faces and vertices
                bm = bmesh.new()
                try:
                    uv_layer = bm.loops.layers.uv.new("uvs")
                    color_layer = bm.loops.layers.color.new("color")

                    bm_verts = []
                    for vertex in vertices:
                        bm_vert = bm.verts.new((vertex.x, vertex.y, vertex.z))
                        bm_verts.append(bm_vert)

                    bm.verts.ensure_lookup_table()
                    bm.faces.ensure_lookup_table()

                    for triangle in triangles:
                        vertex_indices = [idx for idx in triangle]
                        if len(set(vertex_indices)) == 3:
                            try:
                                face = bm.faces.new(
                                    [bm_verts[idx] for idx in vertex_indices]
                                )

                                # assign uv coords and colors
                                triIndex = 0
                                for loop in face.loops:
                                    uv = (
                                        vertices[triangle[triIndex]].u,
                                        1 - vertices[triangle[triIndex]].v,
                                    )
                                    loop[uv_layer].uv = uv
                                    color = (
                                        vertices[triangle[triIndex]].r / 255.0,
                                        vertices[triangle[triIndex]].g / 255.0,
                                        vertices[triangle[triIndex]].b / 255.0,
                                        1.0,
                                    )
                                    loop[color_layer] = color
                                    triIndex += 1
                            except ValueError:
                                # duplicate face, skip over i
                                pass

                    bm.to_mesh(mesh_data)
                    mesh_data.update()
                finally:
                    bm.free()

                self._apply_smooth_normals(mesh_object)

            return True

        except struct.error as e:
            error_msg = f"Failed to read {group_name} draw block data: Invalid data format - {e}"
            self.report({"ERROR"}, error_msg)
            self._import_errors.append(error_msg)
            return False
        except Exception as e:
            error_msg = f"Failed to process {group_name} group: {e}"
            self.report({"ERROR"}, error_msg)
            self._import_errors.append(error_msg)
            return False

    def execute(self, context):
        self._reset_import_state()

        if not self._validate_import_filepath():
            return {"CANCELLED"}

        file = self._open_import_file()
        if file is None:
            return {"CANCELLED"}

        try:
            extra_bank_filename = os.path.basename(self.filepath)[:2] + "gb.bin"
            filename_lower = os.path.basename(self.filepath).lower()
            self.report({"INFO"}, extra_bank_filename)

            _, offsets = self._read_chunk_table(file)

            mesh_chunks = self._find_all_mesh_chunk_offsets(file, offsets)
            slgt_chunk_offset = self._find_slgt_chunk_offset(file, offsets)

            if not mesh_chunks:
                show_message_box(
                    "Could not find any mesh chunks in file.\nThis may not be a valid Silent Hill 4 world mesh file.",
                    "Import Error",
                    "ERROR",
                )
                return {"CANCELLED"}

            self.report({"INFO"}, f"Found {len(mesh_chunks)} mesh chunk(s) in file")

            textures = []
            bank_textures = []

            # special case for padata.bin, the texture chunks need to be handled slightly differntly
            if filename_lower == "padata.bin":
                self.report({"INFO"}, "Detected padata.bin - hack mode enabled")
                texture_chunk_offsets = self._find_all_texture_chunk_offsets(
                    file, offsets
                )
                self.report(
                    {"INFO"}, f"Found {len(texture_chunk_offsets)} texture chunks"
                )

                if len(texture_chunk_offsets) >= 1:
                    if not self.getTexturesFromFile(
                        texture_chunk_offsets[0], textures, file, 0
                    ):
                        self._add_warning(
                            "Failed to load some textures from first texture chunk."
                        )
                    self._read_texture_data(file, textures)
                    self._write_dds_files(textures)

                if len(texture_chunk_offsets) >= 2:
                    # Load second texture chunk as bank 1
                    if not self.getTexturesFromFile(
                        texture_chunk_offsets[1], bank_textures, file, 1
                    ):
                        self._add_warning(
                            "Failed to load some textures from second texture chunk."
                        )
                    self._read_texture_data(file, bank_textures)
                    self._write_dds_files(bank_textures)

            # another special case, this time ofr iwa_teststage, where we hardcode it to use specific texture chunks
            elif filename_lower == "iwa_teststage.bin":
                self.report(
                    {"INFO"},
                    "Detected iwa_teststage.bin - using 2nd chunk (bank 0) and 3rd chunk (bank 1)",
                )
                texture_chunk_offsets = self._find_all_texture_chunk_offsets(
                    file, offsets
                )
                self.report(
                    {"INFO"}, f"Found {len(texture_chunk_offsets)} texture chunks"
                )

                if len(texture_chunk_offsets) >= 2:
                    # Bank 0 = 2nd texture chunk (index 1)
                    if not self.getTexturesFromFile(
                        texture_chunk_offsets[1], textures, file, 0
                    ):
                        self._add_warning(
                            "Failed to load textures from second texture chunk (bank 0)."
                        )
                    self._read_texture_data(file, textures)
                    self._write_dds_files(textures)
                else:
                    self._add_warning(
                        f"iwa_teststage.bin: Expected 2+ texture chunks, found {len(texture_chunk_offsets)}"
                    )

                if len(texture_chunk_offsets) >= 3:
                    # Bank 1 = 3rd texture chunk (index 2)
                    if not self.getTexturesFromFile(
                        texture_chunk_offsets[2], bank_textures, file, 1
                    ):
                        self._add_warning(
                            "Failed to load textures from third texture chunk (bank 1)."
                        )
                    self._read_texture_data(file, bank_textures)
                    self._write_dds_files(bank_textures)
                else:
                    self._add_warning(
                        f"iwa_teststage.bin: Expected 3+ texture chunks for bank 1, found {len(texture_chunk_offsets)}"
                    )

            else:
                # Normal case: single texture chunk + optional external bank file
                texture_chunk_offset = self._find_texture_chunk_offset(file, offsets)

                if texture_chunk_offset == 0:
                    warning_msg = "No texture chunk found - meshes will be imported without textures."
                    self.report({"WARNING"}, warning_msg)
                    self._add_warning(warning_msg)
                else:
                    if not self.getTexturesFromFile(
                        texture_chunk_offset, textures, file, 0
                    ):
                        self._add_warning(
                            "Failed to load some textures from main file."
                        )

                self._read_texture_data(file, textures)
                self._write_dds_files(textures)

                bank_textures = self._try_load_bank_textures(extra_bank_filename)

            filename_base = os.path.splitext(os.path.basename(self.filepath))[0]
            root_collection = bpy.data.collections.new(f"SH4_{filename_base}")
            bpy.context.scene.collection.children.link(root_collection)

            imported_count = 0
            for mesh_index, (chunk_index, mesh_chunk_offset) in enumerate(mesh_chunks):
                is_valid, chunk_version = self._validate_mesh_chunk_header(
                    file, mesh_chunk_offset, chunk_index
                )
                if not is_valid:
                    self._add_warning(
                        f"Skipping invalid mesh chunk at index {chunk_index}"
                    )
                    continue

                version_suffix = f"_V{chunk_version}" if chunk_version == 1 else ""
                mesh_collection = bpy.data.collections.new(
                    f"WorldMesh_{mesh_index:02d}_Chunk{chunk_index:02d}{version_suffix}"
                )
                root_collection.children.link(mesh_collection)

                self._import_mesh_groups(
                    file,
                    mesh_chunk_offset,
                    textures,
                    bank_textures,
                    mesh_index,
                    chunk_index,
                    mesh_collection,
                    chunk_version,
                )
                imported_count += 1

            self.report(
                {"INFO"},
                f"Successfully imported {imported_count} of {len(mesh_chunks)} mesh chunks",
            )
            # disable light importing (for now)
            # TODO: make this work eventually
            # self._import_lights_from_slgt(file, slgt_chunk_offset)

        finally:
            file.close()

        self._show_import_summary()
        if self._import_errors:
            self.report({"WARNING"}, "Import finished with errors (see popup)")
        elif self._import_warnings:
            self.report({"INFO"}, "Import finished with warnings (see popup)")
        else:
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
