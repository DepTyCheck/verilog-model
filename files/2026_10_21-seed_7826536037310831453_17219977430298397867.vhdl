-- Seed: 7826536037310831453,17219977430298397867

entity nrusu is
  port (nsyf : inout boolean_vector(1 downto 0));
end nrusu;

architecture jkfdpvr of nrusu is
  
begin
  -- Single-driven assignments
  nsyf <= (TRUE, TRUE);
end jkfdpvr;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (wqxgp : in real_vector(4 to 1); cuoxdr : in integer; anqryuinn : in std_logic; wrxyrivtj : buffer integer);
end v;

architecture pkvlh of v is
  signal igwpzji : boolean_vector(1 downto 0);
begin
  lfifgberd : entity work.nrusu
    port map (nsyf => igwpzji);
end pkvlh;

entity mtnc is
  port (cijeyaxp : in integer; aultm : out time; hfsankihy : buffer severity_level);
end mtnc;

library ieee;
use ieee.std_logic_1164.all;

architecture dob of mtnc is
  signal ojfvzr : integer;
  signal mg : std_logic;
  signal xuxs : integer;
  signal ayjjhdu : real_vector(4 to 1);
  signal zslje : boolean_vector(1 downto 0);
  signal qmewxvgz : boolean_vector(1 downto 0);
begin
  ziqqs : entity work.nrusu
    port map (nsyf => qmewxvgz);
  o : entity work.nrusu
    port map (nsyf => zslje);
  zlybgc : entity work.v
    port map (wqxgp => ayjjhdu, cuoxdr => xuxs, anqryuinn => mg, wrxyrivtj => ojfvzr);
  
  -- Single-driven assignments
  xuxs <= 304;
  aultm <= 8#753# ms;
  hfsankihy <= hfsankihy;
  ayjjhdu <= ayjjhdu;
  
  -- Multi-driven assignments
  mg <= 'U';
  mg <= mg;
  mg <= mg;
end dob;



-- Seed after: 624984237538265858,17219977430298397867
