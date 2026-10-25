-- Seed: 13797673614535492660,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity whsbtt is
  port (q : linkage time; mauxiezvr : in std_logic_vector(1 downto 4); uaxtmgs : in std_logic);
end whsbtt;

architecture ro of whsbtt is
  
begin
  
end ro;

library ieee;
use ieee.std_logic_1164.all;

entity zgwbsf is
  port (z : linkage std_logic_vector(2 downto 0); ynbvz : in time; aesfj : linkage time_vector(2 downto 2));
end zgwbsf;

library ieee;
use ieee.std_logic_1164.all;

architecture gpu of zgwbsf is
  signal phhpmliokl : time;
  signal taixsjrwjn : std_logic_vector(1 downto 4);
  signal wm : time;
  signal gyfz : std_logic;
  signal mpwveutm : std_logic_vector(1 downto 4);
  signal bwupopv : time;
begin
  i : entity work.whsbtt
    port map (q => bwupopv, mauxiezvr => mpwveutm, uaxtmgs => gyfz);
  ohjcfuxh : entity work.whsbtt
    port map (q => wm, mauxiezvr => taixsjrwjn, uaxtmgs => gyfz);
  xzuel : entity work.whsbtt
    port map (q => phhpmliokl, mauxiezvr => taixsjrwjn, uaxtmgs => gyfz);
  
  -- Multi-driven assignments
  gyfz <= gyfz;
  mpwveutm <= (others => '0');
  mpwveutm <= "";
end gpu;



-- Seed after: 6347823811680448028,13788294582928374589
