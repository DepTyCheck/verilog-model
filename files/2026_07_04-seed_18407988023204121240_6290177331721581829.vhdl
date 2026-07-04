-- Seed: 18407988023204121240,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;

entity rcccxa is
  port (gu : out real; tzhivg : inout std_logic_vector(2 to 3));
end rcccxa;

architecture l of rcccxa is
  
begin
  -- Multi-driven assignments
  tzhivg <= tzhivg;
end l;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity pxkt is
  port (jwqrudstt : inout array_value_mirror; og : linkage std_logic_vector(2 downto 4));
end pxkt;

library ieee;
use ieee.std_logic_1164.all;

architecture kfat of pxkt is
  signal piuxagbuuo : std_logic_vector(2 to 3);
  signal x : real;
  signal zy : std_logic_vector(2 to 3);
  signal rrzassx : real;
  signal osquvkidnu : std_logic_vector(2 to 3);
  signal eycxn : real;
begin
  gjd : entity work.rcccxa
    port map (gu => eycxn, tzhivg => osquvkidnu);
  atrrhjb : entity work.rcccxa
    port map (gu => rrzassx, tzhivg => zy);
  zodpffj : entity work.rcccxa
    port map (gu => x, tzhivg => piuxagbuuo);
  
  -- Multi-driven assignments
  zy <= osquvkidnu;
end kfat;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity e is
  port (pfozyzdxrl : in time; kzpqfmvxu : inout record_subtype_mirror; wnhwwd : inout integer_value_mirror; ptb : in std_logic_vector(3 to 4));
end e;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture iw of e is
  signal xihxg : std_logic_vector(2 to 3);
  signal pvvyup : real;
  signal kcpv : std_logic_vector(2 downto 4);
  shared variable fnwml : array_value_mirror;
  signal tbngpsqm : std_logic_vector(2 to 3);
  signal jiod : real;
  signal nrigxzrmg : std_logic_vector(2 to 3);
  signal qv : real;
begin
  ipibqh : entity work.rcccxa
    port map (gu => qv, tzhivg => nrigxzrmg);
  m : entity work.rcccxa
    port map (gu => jiod, tzhivg => tbngpsqm);
  ctaeun : entity work.pxkt
    port map (jwqrudstt => fnwml, og => kcpv);
  qkdomzofg : entity work.rcccxa
    port map (gu => pvvyup, tzhivg => xihxg);
  
  -- Multi-driven assignments
  nrigxzrmg <= ptb;
  nrigxzrmg <= ('W', 'X');
  xihxg <= ptb;
end iw;



-- Seed after: 12228453581403980177,6290177331721581829
