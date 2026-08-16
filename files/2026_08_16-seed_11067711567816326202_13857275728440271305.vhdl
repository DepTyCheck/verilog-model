-- Seed: 11067711567816326202,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity uxuit is
  port (kuad : out std_logic_vector(4 downto 4); gbjwqudtfy : linkage boolean_vector(3 downto 1); iddd : linkage std_logic; leabyihzq : in time);
end uxuit;

architecture hmwqcmez of uxuit is
  
begin
  -- Multi-driven assignments
  kuad <= (others => 'X');
end hmwqcmez;

library ieee;
use ieee.std_logic_1164.all;

entity ka is
  port (gaytatjj : inout integer; rnh : in std_logic; holmr : out std_logic);
end ka;

library ieee;
use ieee.std_logic_1164.all;

architecture ssvl of ka is
  signal ixasvcdpp : time;
  signal rvuzfh : std_logic;
  signal hgyg : boolean_vector(3 downto 1);
  signal xfvkmtiod : std_logic_vector(4 downto 4);
begin
  amucoim : entity work.uxuit
    port map (kuad => xfvkmtiod, gbjwqudtfy => hgyg, iddd => rvuzfh, leabyihzq => ixasvcdpp);
  
  -- Single-driven assignments
  ixasvcdpp <= ixasvcdpp;
  gaytatjj <= 16#61#;
  
  -- Multi-driven assignments
  rvuzfh <= holmr;
  holmr <= 'X';
  rvuzfh <= 'Z';
  rvuzfh <= 'W';
end ssvl;

library ieee;
use ieee.std_logic_1164.all;

entity kamt is
  port (gjkiibliob : out std_logic; e : inout boolean; x : out boolean; vxxjvmw : out integer);
end kamt;

library ieee;
use ieee.std_logic_1164.all;

architecture jkcm of kamt is
  signal wfdajis : time;
  signal kobwl : boolean_vector(3 downto 1);
  signal ftouoxj : integer;
  signal voozdve : time;
  signal qsdne : boolean_vector(3 downto 1);
  signal pbmxwk : std_logic_vector(4 downto 4);
begin
  pbyrc : entity work.uxuit
    port map (kuad => pbmxwk, gbjwqudtfy => qsdne, iddd => gjkiibliob, leabyihzq => voozdve);
  zi : entity work.ka
    port map (gaytatjj => ftouoxj, rnh => gjkiibliob, holmr => gjkiibliob);
  iyqxojp : entity work.uxuit
    port map (kuad => pbmxwk, gbjwqudtfy => kobwl, iddd => gjkiibliob, leabyihzq => wfdajis);
  
  -- Multi-driven assignments
  gjkiibliob <= '1';
  gjkiibliob <= 'X';
end jkcm;

entity q is
  port (niraeuoysh : buffer time; ojo : buffer real; kzwzegcbyj : inout time_vector(2 downto 0));
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture fnoujwbjbb of q is
  signal w : boolean_vector(3 downto 1);
  signal izangpuatm : std_logic_vector(4 downto 4);
  signal vrwkkbe : integer;
  signal vviiy : boolean;
  signal qmn : boolean;
  signal rovqx : std_logic;
begin
  qwrucq : entity work.kamt
    port map (gjkiibliob => rovqx, e => qmn, x => vviiy, vxxjvmw => vrwkkbe);
  dvpkrop : entity work.uxuit
    port map (kuad => izangpuatm, gbjwqudtfy => w, iddd => rovqx, leabyihzq => niraeuoysh);
  
  -- Single-driven assignments
  kzwzegcbyj <= (3414 ps, 2003 us, 4_4 ns);
  
  -- Multi-driven assignments
  rovqx <= rovqx;
  rovqx <= rovqx;
  rovqx <= 'U';
  rovqx <= rovqx;
end fnoujwbjbb;



-- Seed after: 14187599950601959755,13857275728440271305
