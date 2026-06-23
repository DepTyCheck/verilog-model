-- Seed: 997066112106279240,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity mqfox is
  port (ldhe : inout real; hcgfof : linkage std_logic; l : buffer boolean);
end mqfox;

architecture nspokgsheb of mqfox is
  
begin
  -- Single-driven assignments
  l <= FALSE;
  ldhe <= 1.2200;
end nspokgsheb;

entity bytvtdg is
  port (rlllzpdnd : inout real; qpkk : out bit_vector(4 to 0));
end bytvtdg;

library ieee;
use ieee.std_logic_1164.all;

architecture u of bytvtdg is
  signal fhmxnauzv : boolean;
  signal fpq : boolean;
  signal oesxzobay : std_logic;
  signal rdo : real;
  signal nezqbeuq : boolean;
  signal ebmvga : std_logic;
  signal inwiep : real;
begin
  jyrrluojb : entity work.mqfox
    port map (ldhe => inwiep, hcgfof => ebmvga, l => nezqbeuq);
  uex : entity work.mqfox
    port map (ldhe => rdo, hcgfof => oesxzobay, l => fpq);
  mqgcgzy : entity work.mqfox
    port map (ldhe => rlllzpdnd, hcgfof => ebmvga, l => fhmxnauzv);
  
  -- Single-driven assignments
  qpkk <= (others => '0');
end u;

entity yumtxcnvx is
  port (oq : in time; hmkcyyu : buffer time; hpqmzbkpg : inout bit);
end yumtxcnvx;

library ieee;
use ieee.std_logic_1164.all;

architecture hxvx of yumtxcnvx is
  signal eba : boolean;
  signal sadifwfds : std_logic;
  signal ijqifs : real;
begin
  wxiu : entity work.mqfox
    port map (ldhe => ijqifs, hcgfof => sadifwfds, l => eba);
  
  -- Single-driven assignments
  hpqmzbkpg <= '0';
  hmkcyyu <= 3 min;
  
  -- Multi-driven assignments
  sadifwfds <= '-';
end hxvx;



-- Seed after: 12750283805114024914,8421704836678237495
