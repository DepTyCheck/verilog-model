-- Seed: 892644273165290186,4860866131898729603

entity eievg is
  port (hynz : inout boolean);
end eievg;

architecture jxycltaa of eievg is
  
begin
  -- Single-driven assignments
  hynz <= FALSE;
end jxycltaa;

entity rrgicyrx is
  port (p : in integer_vector(0 to 1); u : inout time);
end rrgicyrx;

architecture gnjxxpnf of rrgicyrx is
  signal xw : boolean;
  signal lfnxgokuej : boolean;
  signal qrttidti : boolean;
begin
  grycpgidwu : entity work.eievg
    port map (hynz => qrttidti);
  fiwz : entity work.eievg
    port map (hynz => lfnxgokuej);
  tzjdqvgro : entity work.eievg
    port map (hynz => xw);
  
  -- Single-driven assignments
  u <= 16#7_3# us;
end gnjxxpnf;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (ycqcnlgj : buffer std_logic);
end c;

architecture wfozdtsor of c is
  signal ssveeqbp : boolean;
begin
  rufl : entity work.eievg
    port map (hynz => ssveeqbp);
  
  -- Multi-driven assignments
  ycqcnlgj <= 'L';
end wfozdtsor;

library ieee;
use ieee.std_logic_1164.all;

entity bjdiv is
  port (bbhyw : in integer; esvnqsldc : out std_logic);
end bjdiv;

architecture vpzickpjpx of bjdiv is
  signal u : time;
  signal azipukkae : integer_vector(0 to 1);
  signal wnjtrcew : boolean;
begin
  swusjcghje : entity work.eievg
    port map (hynz => wnjtrcew);
  gq : entity work.rrgicyrx
    port map (p => azipukkae, u => u);
  
  -- Single-driven assignments
  azipukkae <= (16#0_D_2#, 16#999AB#);
end vpzickpjpx;



-- Seed after: 4163535804757381793,4860866131898729603
