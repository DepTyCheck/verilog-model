-- Seed: 7824367366106949981,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity cl is
  port (gdncn : out std_logic; gdcm : buffer std_logic_vector(0 to 4));
end cl;

architecture gcsavpaphv of cl is
  
begin
  -- Multi-driven assignments
  gdcm <= ('Z', '0', 'X', 'L', 'X');
end gcsavpaphv;

entity uwovnspb is
  port (vv : linkage bit_vector(1 downto 3); jtszncwmyf : linkage time_vector(2 to 4); snxmo : buffer severity_level; txgqve : out real);
end uwovnspb;

library ieee;
use ieee.std_logic_1164.all;

architecture iojro of uwovnspb is
  signal bin : std_logic;
  signal x : std_logic;
  signal xfiskuasko : std_logic_vector(0 to 4);
  signal vrie : std_logic;
begin
  mrxl : entity work.cl
    port map (gdncn => vrie, gdcm => xfiskuasko);
  nubmkqhjh : entity work.cl
    port map (gdncn => x, gdcm => xfiskuasko);
  ty : entity work.cl
    port map (gdncn => bin, gdcm => xfiskuasko);
  uohdnxsczy : entity work.cl
    port map (gdncn => vrie, gdcm => xfiskuasko);
  
  -- Multi-driven assignments
  x <= vrie;
  vrie <= x;
end iojro;



-- Seed after: 15677348602014460715,499459191852795575
