-- Seed: 17266980172473800754,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity xswc is
  port (j : out std_logic);
end xswc;

architecture tq of xswc is
  
begin
  -- Multi-driven assignments
  j <= '-';
  j <= j;
end tq;

library ieee;
use ieee.std_logic_1164.all;

entity qhenhxrh is
  port (zyelkocs : linkage std_logic_vector(2 downto 3); fuz : linkage time);
end qhenhxrh;

architecture klpefm of qhenhxrh is
  
begin
  
end klpefm;

library ieee;
use ieee.std_logic_1164.all;

entity gelbzsbd is
  port (mhs : linkage time_vector(3 downto 3); xvksobt : out real; zmbespcul : linkage std_logic_vector(2 to 3));
end gelbzsbd;

library ieee;
use ieee.std_logic_1164.all;

architecture tvcxbgc of gelbzsbd is
  signal ckpn : std_logic;
  signal smkodylcga : time;
  signal tuodfs : std_logic_vector(2 downto 3);
begin
  hbnvwh : entity work.qhenhxrh
    port map (zyelkocs => tuodfs, fuz => smkodylcga);
  u : entity work.xswc
    port map (j => ckpn);
  
  -- Multi-driven assignments
  tuodfs <= (others => '0');
  tuodfs <= "";
end tvcxbgc;



-- Seed after: 7560096450869737366,8891552411914730853
