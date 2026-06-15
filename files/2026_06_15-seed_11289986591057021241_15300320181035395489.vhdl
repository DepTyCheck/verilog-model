-- Seed: 11289986591057021241,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity gfs is
  port (m : out character; d : out std_logic_vector(4 downto 3); yjywqudrc : linkage std_logic; ompqiztpqs : inout integer_vector(2 downto 1));
end gfs;

architecture qrttanlpza of gfs is
  
begin
  -- Multi-driven assignments
  d <= ('W', '1');
end qrttanlpza;

entity epeg is
  port (bafmchy : in time; jkdrdvl : out time; edacnxkkt : inout boolean_vector(3 downto 2));
end epeg;

library ieee;
use ieee.std_logic_1164.all;

architecture pczu of epeg is
  signal iagzo : integer_vector(2 downto 1);
  signal hosv : std_logic;
  signal iffkqlmfvk : std_logic_vector(4 downto 3);
  signal nbv : character;
begin
  tdavdpn : entity work.gfs
    port map (m => nbv, d => iffkqlmfvk, yjywqudrc => hosv, ompqiztpqs => iagzo);
  
  -- Single-driven assignments
  jkdrdvl <= 1 hr;
  edacnxkkt <= (FALSE, TRUE);
end pczu;



-- Seed after: 12684143166792586826,15300320181035395489
