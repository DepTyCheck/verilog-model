-- Seed: 10486613570728701356,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity vrntemyrrx is
  port (mtricului : inout time; mrt : buffer real; cefhz : in time; xhnazyfolo : out std_logic_vector(2 to 4));
end vrntemyrrx;

architecture zoygtpknv of vrntemyrrx is
  
begin
  -- Single-driven assignments
  mrt <= 4_0_0_0.1_3_3;
  mtricului <= cefhz;
end zoygtpknv;

library ieee;
use ieee.std_logic_1164.all;

entity zzkuok is
  port (njnd : inout character; ruyqzhnm : inout std_logic);
end zzkuok;

library ieee;
use ieee.std_logic_1164.all;

architecture b of zzkuok is
  signal mxjwdmuj : std_logic_vector(2 to 4);
  signal bcuenexi : time;
  signal fpkr : real;
  signal m : time;
  signal qu : real;
  signal lggwe : time;
  signal ffex : real;
  signal a : std_logic_vector(2 to 4);
  signal v : time;
  signal zyy : real;
  signal uncidn : time;
begin
  ctqaxgovdd : entity work.vrntemyrrx
    port map (mtricului => uncidn, mrt => zyy, cefhz => v, xhnazyfolo => a);
  bgtfxxvjva : entity work.vrntemyrrx
    port map (mtricului => v, mrt => ffex, cefhz => lggwe, xhnazyfolo => a);
  czjka : entity work.vrntemyrrx
    port map (mtricului => lggwe, mrt => qu, cefhz => v, xhnazyfolo => a);
  wtvyuoa : entity work.vrntemyrrx
    port map (mtricului => m, mrt => fpkr, cefhz => bcuenexi, xhnazyfolo => mxjwdmuj);
  
  -- Multi-driven assignments
  ruyqzhnm <= ruyqzhnm;
  ruyqzhnm <= ruyqzhnm;
  ruyqzhnm <= ruyqzhnm;
end b;



-- Seed after: 8684347771016064083,10940991575366938685
