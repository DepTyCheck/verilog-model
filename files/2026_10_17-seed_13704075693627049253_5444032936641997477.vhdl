-- Seed: 13704075693627049253,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity uiwdmi is
  port (gy : inout boolean; dqv : out time; vzkfpwsjfx : inout std_logic_vector(0 downto 4); lcztjvzvu : out real);
end uiwdmi;

architecture xmtmn of uiwdmi is
  
begin
  -- Single-driven assignments
  gy <= gy;
  lcztjvzvu <= 16#D.A3#;
  
  -- Multi-driven assignments
  vzkfpwsjfx <= (others => '0');
end xmtmn;

entity riwfbnvg is
  port (j : out time; sao : buffer boolean; ub : inout integer);
end riwfbnvg;

library ieee;
use ieee.std_logic_1164.all;

architecture ravwemzv of riwfbnvg is
  signal xkk : real;
  signal sjdu : real;
  signal soxgzoa : std_logic_vector(0 downto 4);
  signal q : time;
  signal f : boolean;
  signal dvnfmft : real;
  signal ltyfc : std_logic_vector(0 downto 4);
  signal ash : time;
  signal gbb : boolean;
  signal oict : real;
  signal utbd : std_logic_vector(0 downto 4);
  signal rlp : time;
  signal etxmy : boolean;
begin
  wmzgrlbb : entity work.uiwdmi
    port map (gy => etxmy, dqv => rlp, vzkfpwsjfx => utbd, lcztjvzvu => oict);
  egkwicb : entity work.uiwdmi
    port map (gy => gbb, dqv => ash, vzkfpwsjfx => ltyfc, lcztjvzvu => dvnfmft);
  djxqaajy : entity work.uiwdmi
    port map (gy => f, dqv => q, vzkfpwsjfx => soxgzoa, lcztjvzvu => sjdu);
  whuu : entity work.uiwdmi
    port map (gy => sao, dqv => j, vzkfpwsjfx => soxgzoa, lcztjvzvu => xkk);
  
  -- Single-driven assignments
  ub <= ub;
end ravwemzv;



-- Seed after: 15169530953076505860,5444032936641997477
