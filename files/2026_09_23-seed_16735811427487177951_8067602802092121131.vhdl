-- Seed: 16735811427487177951,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity azhrxoi is
  port (iyvqnem : buffer real; zkthqb : inout std_logic; fgfabu : inout std_logic; wzxbvzk : in real);
end azhrxoi;

architecture ube of azhrxoi is
  
begin
  -- Single-driven assignments
  iyvqnem <= 16#C_6_4_B_0.1_C_0_A_3#;
  
  -- Multi-driven assignments
  fgfabu <= '1';
  fgfabu <= zkthqb;
  fgfabu <= 'X';
  fgfabu <= fgfabu;
end ube;

entity qnnehzni is
  port (wpeeuhziv : out time; ikliq : buffer real; qureyfdl : buffer time);
end qnnehzni;

library ieee;
use ieee.std_logic_1164.all;

architecture mzt of qnnehzni is
  signal gfbobpgpia : real;
  signal qdq : std_logic;
  signal tsldri : real;
  signal jazn : std_logic;
  signal wesckagbzu : real;
begin
  kgwbjan : entity work.azhrxoi
    port map (iyvqnem => wesckagbzu, zkthqb => jazn, fgfabu => jazn, wzxbvzk => tsldri);
  lhvojo : entity work.azhrxoi
    port map (iyvqnem => tsldri, zkthqb => jazn, fgfabu => jazn, wzxbvzk => ikliq);
  kz : entity work.azhrxoi
    port map (iyvqnem => ikliq, zkthqb => qdq, fgfabu => jazn, wzxbvzk => gfbobpgpia);
  
  -- Single-driven assignments
  qureyfdl <= 1 sec;
  gfbobpgpia <= 224.3_0;
  wpeeuhziv <= qureyfdl;
  
  -- Multi-driven assignments
  jazn <= 'H';
  jazn <= qdq;
  jazn <= 'W';
  jazn <= jazn;
end mzt;



-- Seed after: 11322668880480914477,8067602802092121131
