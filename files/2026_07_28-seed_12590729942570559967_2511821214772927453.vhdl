-- Seed: 12590729942570559967,2511821214772927453

entity apfwix is
  port (axrs : in integer; qosshx : in bit; oqxuh : buffer time; nocz : linkage integer);
end apfwix;

architecture ee of apfwix is
  
begin
  -- Single-driven assignments
  oqxuh <= 0_1_2_1_1 ms;
end ee;

entity sdxbt is
  port (y : linkage integer; jhez : out integer_vector(0 downto 3));
end sdxbt;

architecture qosdmsim of sdxbt is
  
begin
  -- Single-driven assignments
  jhez <= (others => 0);
end qosdmsim;

library ieee;
use ieee.std_logic_1164.all;

entity vollf is
  port (dzhp : inout std_logic_vector(1 to 2); ihwmtxlvot : out std_logic);
end vollf;

architecture cemottjoz of vollf is
  
begin
  -- Multi-driven assignments
  ihwmtxlvot <= '-';
  ihwmtxlvot <= 'L';
end cemottjoz;

library ieee;
use ieee.std_logic_1164.all;

entity aeflj is
  port (yh : inout std_logic_vector(0 downto 4));
end aeflj;

library ieee;
use ieee.std_logic_1164.all;

architecture whcbzhka of aeflj is
  signal hzvjccanz : time;
  signal vrxl : bit;
  signal illkiib : integer;
  signal wzwjznmv : std_logic;
  signal gkezzgrwl : std_logic_vector(1 to 2);
begin
  rin : entity work.vollf
    port map (dzhp => gkezzgrwl, ihwmtxlvot => wzwjznmv);
  m : entity work.apfwix
    port map (axrs => illkiib, qosshx => vrxl, oqxuh => hzvjccanz, nocz => illkiib);
  
  -- Single-driven assignments
  vrxl <= vrxl;
  
  -- Multi-driven assignments
  yh <= "";
  gkezzgrwl <= ('W', '0');
  gkezzgrwl <= ('W', '-');
end whcbzhka;



-- Seed after: 15093406235407635919,2511821214772927453
