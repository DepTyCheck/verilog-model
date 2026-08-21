-- Seed: 1094885181753962621,16188444798499499427

entity lvzxigh is
  port (ivd : out time; xrcxk : buffer time);
end lvzxigh;

architecture okoxow of lvzxigh is
  
begin
  
end okoxow;

library ieee;
use ieee.std_logic_1164.all;

entity aaxxlyzv is
  port (oyvzcgz : inout std_logic_vector(2 to 3));
end aaxxlyzv;

architecture tnfeccvckx of aaxxlyzv is
  signal wib : time;
  signal wc : time;
  signal ydtyz : time;
  signal psyqvev : time;
  signal hqd : time;
  signal cf : time;
  signal btnc : time;
  signal auwio : time;
begin
  pdc : entity work.lvzxigh
    port map (ivd => auwio, xrcxk => btnc);
  rk : entity work.lvzxigh
    port map (ivd => cf, xrcxk => hqd);
  dmm : entity work.lvzxigh
    port map (ivd => psyqvev, xrcxk => ydtyz);
  f : entity work.lvzxigh
    port map (ivd => wc, xrcxk => wib);
  
  -- Multi-driven assignments
  oyvzcgz <= "10";
  oyvzcgz <= oyvzcgz;
  oyvzcgz <= oyvzcgz;
end tnfeccvckx;



-- Seed after: 6398230630231611922,16188444798499499427
