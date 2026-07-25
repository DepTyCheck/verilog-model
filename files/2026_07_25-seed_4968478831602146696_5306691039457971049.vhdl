-- Seed: 4968478831602146696,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (jswhlvg : linkage bit_vector(3 to 2); wfvmtmm : in integer; psuupkhzh : linkage std_logic_vector(0 downto 1));
end x;

architecture xdvlffhazp of x is
  
begin
  
end xdvlffhazp;

entity aqnb is
  port (xvzsmusxw : inout boolean);
end aqnb;

library ieee;
use ieee.std_logic_1164.all;

architecture kj of aqnb is
  signal fbjqxuxvj : std_logic_vector(0 downto 1);
  signal tmqdyxhw : integer;
  signal vpo : bit_vector(3 to 2);
begin
  ffeseffv : entity work.x
    port map (jswhlvg => vpo, wfvmtmm => tmqdyxhw, psuupkhzh => fbjqxuxvj);
  
  -- Single-driven assignments
  xvzsmusxw <= FALSE;
  tmqdyxhw <= 0_2;
  
  -- Multi-driven assignments
  fbjqxuxvj <= fbjqxuxvj;
  fbjqxuxvj <= fbjqxuxvj;
  fbjqxuxvj <= fbjqxuxvj;
  fbjqxuxvj <= (others => '0');
end kj;

entity tkojnj is
  port (i : linkage time);
end tkojnj;

library ieee;
use ieee.std_logic_1164.all;

architecture jqmkozviwx of tkojnj is
  signal fpstsoyjcg : boolean;
  signal heb : std_logic_vector(0 downto 1);
  signal ntfoh : integer;
  signal ppjtgxzki : bit_vector(3 to 2);
begin
  z : entity work.x
    port map (jswhlvg => ppjtgxzki, wfvmtmm => ntfoh, psuupkhzh => heb);
  jorajkun : entity work.aqnb
    port map (xvzsmusxw => fpstsoyjcg);
  
  -- Single-driven assignments
  ntfoh <= 2#0_1_0_1#;
  
  -- Multi-driven assignments
  heb <= heb;
  heb <= heb;
  heb <= heb;
end jqmkozviwx;



-- Seed after: 18031514557752405424,5306691039457971049
