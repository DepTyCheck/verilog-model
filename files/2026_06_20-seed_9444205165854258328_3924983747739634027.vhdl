-- Seed: 9444205165854258328,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity upgw is
  port (ywn : out std_logic_vector(2 to 3); ic : linkage integer; ltllnr : linkage std_logic);
end upgw;

architecture bblafcqczb of upgw is
  
begin
  -- Multi-driven assignments
  ywn <= "XH";
  ywn <= "11";
end bblafcqczb;

entity cxeanq is
  port (cdhkdkrb : linkage real; eypx : out time);
end cxeanq;

library ieee;
use ieee.std_logic_1164.all;

architecture bmunbkqrna of cxeanq is
  signal p : std_logic;
  signal wp : integer;
  signal iudz : std_logic;
  signal muhfhwps : integer;
  signal mp : std_logic_vector(2 to 3);
  signal gmlzzozbmn : std_logic;
  signal v : integer;
  signal bvndgmjnia : std_logic_vector(2 to 3);
begin
  jioweeqp : entity work.upgw
    port map (ywn => bvndgmjnia, ic => v, ltllnr => gmlzzozbmn);
  zncmgp : entity work.upgw
    port map (ywn => mp, ic => muhfhwps, ltllnr => iudz);
  mqaf : entity work.upgw
    port map (ywn => mp, ic => wp, ltllnr => p);
  
  -- Single-driven assignments
  eypx <= 2 sec;
end bmunbkqrna;

library ieee;
use ieee.std_logic_1164.all;

entity igsuy is
  port (bslzmysqz : out real; uxqs : in std_logic_vector(2 downto 3); iwhykhht : buffer integer; kgwbfhgps : inout real);
end igsuy;

architecture ymm of igsuy is
  signal c : time;
  signal aw : real;
begin
  kfwbm : entity work.cxeanq
    port map (cdhkdkrb => aw, eypx => c);
  
  -- Single-driven assignments
  kgwbfhgps <= 1_3_0.1;
  bslzmysqz <= 024.213;
  iwhykhht <= 3_0;
end ymm;



-- Seed after: 1723902658243369940,3924983747739634027
