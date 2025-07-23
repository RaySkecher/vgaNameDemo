----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date: 04/25/2014 02:10:40 PM
-- Design Name:
-- Module Name: vga_ctrl - Behavioral
-- Project Name:
-- Target Devices:
-- Tool Versions:
-- Description: VGA Controller that displays black text ("RaySkechers") moving in a
--              circular path on top of a moving color bar background.
--
-- Dependencies: clk_wiz_0 component (for 108MHz pixel clock)
--
-- Revision:
-- Revision 1.2 - Added moving color bar background and multiplexed it with the text layer.
--
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.std_logic_unsigned.all;
use ieee.math_real.all;

entity vga_ctrl is
    Port ( CLK_I : in STD_LOGIC; -- Main system clock
           VGA_HS_O : out STD_LOGIC;
           VGA_VS_O : out STD_LOGIC;
           VGA_RED_O : out STD_LOGIC_VECTOR (3 downto 0);
           VGA_BLUE_O : out STD_LOGIC_VECTOR (3 downto 0);
           VGA_GREEN_O : out STD_LOGIC_VECTOR (3 downto 0)
           );
end vga_ctrl;

architecture Behavioral of vga_ctrl is

component clk_wiz_0
port
  ( clk_in1 : in std_logic;
    clk_out1 : out std_logic );
end component;

  --***1280x1024@60Hz VGA Timings***--
  constant FRAME_WIDTH : natural := 1280;
  constant FRAME_HEIGHT : natural := 1024;
  constant H_FP : natural := 48;
  constant H_PW : natural := 112;
  constant H_MAX : natural := 1688;
  constant V_FP : natural := 1;
  constant V_PW : natural := 3;
  constant V_MAX : natural := 1066;
  constant H_POL : std_logic := '1';
  constant V_POL : std_logic := '1';

  -- Core VGA Signals
  signal pxl_clk : std_logic;
  signal active  : std_logic;
  signal h_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');
  signal v_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');
  signal h_cntr_reg_dly    : std_logic_vector(11 downto 0) := (others => '0');
  signal v_cntr_reg_dly    : std_logic_vector(11 downto 0) := (others => '0');
  signal h_sync_reg, h_sync_reg_dly : std_logic := not(H_POL);
  signal v_sync_reg, v_sync_reg_dly : std_logic := not(V_POL);
  signal vga_red_cmb, vga_green_cmb, vga_blue_cmb  : std_logic_vector(3 downto 0);
  signal vga_red, vga_green, vga_blue    : std_logic_vector(3 downto 0);
  signal vga_red_reg, vga_green_reg, vga_blue_reg  : std_logic_vector(3 downto 0) := (others =>'0');

  -- Text Display Constants
  constant DISPLAY_TEXT : string := "RaySkechers";
  constant TEXT_LENGTH : natural := DISPLAY_TEXT'length;
  constant FONT_CHAR_WIDTH : natural := 8;
  constant FONT_CHAR_HEIGHT : natural := 8;
  constant SCALE_FACTOR : natural := 10;
  constant FONT_WIDTH : natural := FONT_CHAR_WIDTH * SCALE_FACTOR;
  constant FONT_HEIGHT : natural := FONT_CHAR_HEIGHT * SCALE_FACTOR;
  constant TEXT_TOTAL_WIDTH : natural := TEXT_LENGTH * FONT_WIDTH;
  constant TEXT_TOTAL_HEIGHT : natural := FONT_HEIGHT;

  -- Text Display Signals
  signal text_red_sig, text_green_sig, text_blue_sig : std_logic_vector(3 downto 0) := (others => '0');
  signal enable_text_display : std_logic := '0';

  -- Circular Motion Declarations
  constant ROTATION_STEPS : integer := 256;
  constant ROTATION_RADIUS : integer := 200;
  constant LUT_SCALE_FACTOR : real := 256.0;
  constant SCREEN_CENTER_X : integer := FRAME_WIDTH / 2;
  constant SCREEN_CENTER_Y : integer := FRAME_HEIGHT / 2;
  type sine_lut_t is array (0 to ROTATION_STEPS - 1) of integer;
  function init_sine_lut return sine_lut_t is
      variable lut : sine_lut_t;
  begin
      for i in 0 to ROTATION_STEPS - 1 loop
          lut(i) := integer(round(sin(2.0 * 3.14159265358 * real(i) / real(ROTATION_STEPS)) * LUT_SCALE_FACTOR));
      end loop;
      return lut;
  end function;
  constant SINE_LUT : sine_lut_t := init_sine_lut;
  signal text_start_x : integer range -ROTATION_RADIUS to FRAME_WIDTH + ROTATION_RADIUS := 0;
  signal text_start_y : integer range -ROTATION_RADIUS to FRAME_HEIGHT + ROTATION_RADIUS := 0;
  signal angle_counter : integer range 0 to ROTATION_STEPS - 1 := 0;

  -- Text Rendering Pipeline Registers
  signal char_index_reg : integer range 0 to TEXT_LENGTH-1 := 0;
  signal pixel_x_orig_reg : integer range 0 to FONT_CHAR_WIDTH-1 := 0;
  signal pixel_y_orig_reg : integer range 0 to FONT_CHAR_HEIGHT-1 := 0;
  signal is_in_text_area_reg : std_logic := '0';

  -- NEW: Signals and constants for the moving color bar background
  constant NUM_BARS : natural := 8;
  constant BAR_WIDTH : natural := FRAME_WIDTH / NUM_BARS;
  signal colorbar_offset : integer range 0 to H_MAX - 1 := 0;
  signal colorbar_red_sig, colorbar_green_sig, colorbar_blue_sig : std_logic_vector(3 downto 0);

  -- Font ROM
  type font_char_array is array (0 to FONT_CHAR_HEIGHT - 1) of std_logic_vector(FONT_CHAR_WIDTH - 1 downto 0);
  function get_font_pattern (char : character) return font_char_array is
      variable pattern : font_char_array;
  begin
      case char is
          when 'R' => pattern := ("00000000", "01111000", "10000100", "10000100", "11111000", "10100000", "10010000", "10001000");
          when 'a' => pattern := ("00000000", "00000000","00000000", "00111000", "01000100", "01111100", "01000100", "01000100");
          when 'y' => pattern := ("00000000", "00000000", "00000000", "01000100", "01000100", "00101000", "00010000", "00100000");
          when 'S' => pattern := ("00000000", "00111100", "01000000", "01000000", "00111000", "00000100", "01111000", "00000000");
          when 'k' => pattern := ("00000000", "01000000", "01001000", "01010000", "01100000", "01010000", "01001000", "00000000");
          when 'e' => pattern := ("00000000", "00000000", "00111000", "01000100", "01111000", "01000000", "00111000", "00000000");
          when 'c' => pattern := ("00000000", "00000000", "00111000", "01000000", "01000000", "01000000", "00111000", "00000000");
          when 'h' => pattern := ("00000000", "00000000", "01000000", "01000000", "01111000", "01000100", "01000100", "00000000");
          when 'r' => pattern := ("00000000", "00000000", "00110000", "01001000", "01000000", "01000000", "01000000", "00000000");
          when 's' => pattern := ("00000000", "00000000", "00111000", "01000000", "00111000", "00000100", "01111000", "00000000");
          when others => pattern := (others => "00000000");
      end case;
      return pattern;
  end function;

begin

  clk_wiz_0_inst : clk_wiz_0 port map (clk_in1 => CLK_I, clk_out1 => pxl_clk);

  -- VGA Timing Generation
  horiz_counter_proc: process (pxl_clk) begin if rising_edge(pxl_clk) then if (h_cntr_reg = H_MAX - 1) then h_cntr_reg <= (others =>'0'); else h_cntr_reg <= h_cntr_reg + 1; end if; end if; end process;
  vert_counter_proc: process (pxl_clk) begin if rising_edge(pxl_clk) then if (h_cntr_reg = H_MAX - 1) then if (v_cntr_reg = V_MAX - 1) then v_cntr_reg <= (others => '0'); else v_cntr_reg <= v_cntr_reg + 1; end if; end if; end if; end process;
  horiz_sync_proc: process (pxl_clk) begin if rising_edge(pxl_clk) then if (h_cntr_reg >= H_FP + FRAME_WIDTH) and (h_cntr_reg < H_FP + FRAME_WIDTH + H_PW) then h_sync_reg <= not(H_POL); else h_sync_reg <= H_POL; end if; end if; end process;
  vert_sync_proc: process (pxl_clk) begin if rising_edge(pxl_clk) then if (v_cntr_reg >= V_FP + FRAME_HEIGHT) and (v_cntr_reg < V_FP + FRAME_HEIGHT + V_PW) then v_sync_reg <= not(V_POL); else v_sync_reg <= V_POL; end if; end if; end process;
  active <= '1' when h_cntr_reg_dly < FRAME_WIDTH and v_cntr_reg_dly < FRAME_HEIGHT else '0';

  -- NEW: Frame Update Process. Updates all per-frame animations simultaneously.
  frame_update_proc: process(pxl_clk)
  begin
      if (rising_edge(pxl_clk)) then
          -- On the first pixel of a new frame, update animation counters
          if (v_cntr_reg = 0 and h_cntr_reg = 0) then
              angle_counter <= angle_counter + 1; -- Increment text rotation angle
              colorbar_offset <= colorbar_offset + 1; -- Shift color bars
          end if;
      end if;
  end process frame_update_proc;

  -- NEW: Color Bar Generation Logic (Background Layer)
  colorbar_gen_proc: process(h_cntr_reg_dly, colorbar_offset)
      variable bar_index : integer range 0 to NUM_BARS - 1;
  begin
      -- Calculate which color bar the current pixel is in, including the moving offset
      bar_index := (conv_integer(h_cntr_reg_dly) + colorbar_offset) / BAR_WIDTH mod NUM_BARS;

      case bar_index is
          when 0 => colorbar_red_sig <= "1111"; colorbar_green_sig <= "1111"; colorbar_blue_sig <= "1111"; -- White
          when 1 => colorbar_red_sig <= "1111"; colorbar_green_sig <= "1111"; colorbar_blue_sig <= "0000"; -- Yellow
          when 2 => colorbar_red_sig <= "0000"; colorbar_green_sig <= "1111"; colorbar_blue_sig <= "1111"; -- Cyan
          when 3 => colorbar_red_sig <= "0000"; colorbar_green_sig <= "1111"; colorbar_blue_sig <= "0000"; -- Green
          when 4 => colorbar_red_sig <= "1111"; colorbar_green_sig <= "0000"; colorbar_blue_sig <= "1111"; -- Magenta
          when 5 => colorbar_red_sig <= "1111"; colorbar_green_sig <= "0000"; colorbar_blue_sig <= "0000"; -- Red
          when 6 => colorbar_red_sig <= "0000"; colorbar_green_sig <= "0000"; colorbar_blue_sig <= "1111"; -- Blue
          when 7 => colorbar_red_sig <= "0000"; colorbar_green_sig <= "0000"; colorbar_blue_sig <= "0000"; -- Black
          when others => colorbar_red_sig <= "0000"; colorbar_green_sig <= "0000"; colorbar_blue_sig <= "0000";
      end case;
  end process colorbar_gen_proc;

  -- Combinatorial logic to calculate the text's top-left (X,Y) position
  text_start_x <= SCREEN_CENTER_X - (TEXT_TOTAL_WIDTH / 2) + (ROTATION_RADIUS * SINE_LUT((angle_counter + (ROTATION_STEPS/4)) mod ROTATION_STEPS)) / integer(LUT_SCALE_FACTOR);
  text_start_y <= SCREEN_CENTER_Y - (TEXT_TOTAL_HEIGHT / 2) + (ROTATION_RADIUS * SINE_LUT(angle_counter)) / integer(LUT_SCALE_FACTOR);

  -- Pipelined Text Display Logic (Foreground Layer)
  text_coord_calc_proc: process (pxl_clk)
      variable char_index : integer;
      variable pixel_x_in_char_scaled, pixel_y_in_char_scaled : integer;
  begin
      if (rising_edge(pxl_clk)) then
          is_in_text_area_reg <= '0';
          if (conv_integer(h_cntr_reg) >= text_start_x and conv_integer(h_cntr_reg) < (text_start_x + TEXT_TOTAL_WIDTH) and
              conv_integer(v_cntr_reg) >= text_start_y and conv_integer(v_cntr_reg) < (text_start_y + TEXT_TOTAL_HEIGHT)) then
              is_in_text_area_reg <= '1';
              pixel_x_in_char_scaled := conv_integer(h_cntr_reg) - text_start_x;
              pixel_y_in_char_scaled := conv_integer(v_cntr_reg) - text_start_y;
              char_index := pixel_x_in_char_scaled / FONT_WIDTH;
              if char_index < TEXT_LENGTH then char_index_reg <= char_index; end if;
              pixel_x_orig_reg <= (pixel_x_in_char_scaled mod FONT_WIDTH) / SCALE_FACTOR;
              pixel_y_orig_reg <= (pixel_y_in_char_scaled mod FONT_HEIGHT) / SCALE_FACTOR;
          end if;
      end if;
  end process text_coord_calc_proc;

  text_font_lookup_proc: process (pxl_clk)
      variable font_pattern : font_char_array;
  begin
      if (rising_edge(pxl_clk)) then
          enable_text_display <= '0';
          -- NEW: Default text color doesn't matter as it's only active when enable is '1'
          if is_in_text_area_reg = '1' then
              font_pattern := get_font_pattern(DISPLAY_TEXT(char_index_reg + 1));
              if font_pattern(pixel_y_orig_reg)(FONT_CHAR_WIDTH - 1 - pixel_x_orig_reg) = '1' then
                  enable_text_display <= '1';
                  -- NEW: Set text color to BLACK to be visible on the color bars.
                  text_red_sig   <= "0000"; -- Black
                  text_green_sig <= "0000";
                  text_blue_sig  <= "0000";
              end if;
          end if;
      end if;
  end process text_font_lookup_proc;

  -- Register supporting signals
  register_outputs_proc: process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
        h_cntr_reg_dly <= h_cntr_reg;
        v_cntr_reg_dly <= v_cntr_reg;
    end if;
  end process register_outputs_proc;

  -- NEW: Video Layer Multiplexer. This is the key logic for layering.
  -- It selects the text color if the text is enabled, otherwise it shows the background color bar.
  vga_red   <= text_red_sig   when enable_text_display = '1' else colorbar_red_sig;
  vga_green <= text_green_sig when enable_text_display = '1' else colorbar_green_sig;
  vga_blue  <= text_blue_sig  when enable_text_display = '1' else colorbar_blue_sig;

  -- Blank VGA RGB Signals if outside of the active screen area
  vga_red_cmb <= vga_red when active = '1' else (others => '0');
  vga_green_cmb <= vga_green when active = '1' else (others => '0');
  vga_blue_cmb <= vga_blue when active = '1' else (others => '0');

  -- Final output signal registration
  final_vga_reg_proc: process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      v_sync_reg_dly <= v_sync_reg;
      h_sync_reg_dly <= h_sync_reg;
      vga_red_reg    <= vga_red_cmb;
      vga_green_reg  <= vga_green_cmb;
      vga_blue_reg   <= vga_blue_cmb;
    end if;
  end process final_vga_reg_proc;

  -- Assign registered signals to top-level output ports
  VGA_HS_O     <= h_sync_reg_dly;
  VGA_VS_O     <= v_sync_reg_dly;
  VGA_RED_O    <= vga_red_reg;
  VGA_GREEN_O  <= vga_green_reg;
  VGA_BLUE_O   <= vga_blue_reg;

end Behavioral;