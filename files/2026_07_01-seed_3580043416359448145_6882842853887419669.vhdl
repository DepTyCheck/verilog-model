-- Seed: 3580043416359448145,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity gmslzd is
  port (blnkbmnwj : out std_logic_vector(3 downto 0); cvdxkrw : out string(3 downto 5); fig : inout std_logic; f : in std_logic_vector(2 downto 1));
end gmslzd;

architecture gqjta of gmslzd is
  
begin
  -- Multi-driven assignments
  blnkbmnwj <= "Z-XU";
  fig <= '0';
  fig <= '1';
end gqjta;

library ieee;
use ieee.std_logic_1164.all;

entity fojngb is
  port (znj : out std_logic_vector(0 downto 4); fmyygaatqi : in integer; sfpndia : buffer time; dajt : out std_logic_vector(4 to 0));
end fojngb;

library ieee;
use ieee.std_logic_1164.all;

architecture lcvyqiu of fojngb is
  signal yxcriyp : std_logic_vector(2 downto 1);
  signal iyhiawhoz : string(3 downto 5);
  signal hkkzutvj : std_logic_vector(3 downto 0);
  signal fzjt : std_logic_vector(2 downto 1);
  signal datr : std_logic;
  signal xcokuo : string(3 downto 5);
  signal kgeahcbu : std_logic_vector(3 downto 0);
begin
  odmwpgod : entity work.gmslzd
    port map (blnkbmnwj => kgeahcbu, cvdxkrw => xcokuo, fig => datr, f => fzjt);
  llxuotwhxo : entity work.gmslzd
    port map (blnkbmnwj => hkkzutvj, cvdxkrw => iyhiawhoz, fig => datr, f => yxcriyp);
  
  -- Single-driven assignments
  sfpndia <= 16#D_4_E_3# fs;
  
  -- Multi-driven assignments
  dajt <= "";
end lcvyqiu;



-- Seed after: 15817226106669582174,6882842853887419669
