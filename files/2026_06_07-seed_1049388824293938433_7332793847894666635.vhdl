-- Seed: 1049388824293938433,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity qpd is
  port (ilf : inout time_vector(1 to 2); znowptj : inout std_logic; ptjglun : inout std_logic);
end qpd;



architecture lzcrmoivj of qpd is
  
begin
  
end lzcrmoivj;



entity tl is
  port (hzjccrea : buffer boolean_vector(4 to 2); a : buffer time; aejzak : out time; pt : linkage real);
end tl;

library ieee;
use ieee.std_logic_1164.all;

architecture agpnmfq of tl is
  signal sjochdx : std_logic;
  signal dlf : std_logic;
  signal t : time_vector(1 to 2);
begin
  aiw : entity work.qpd
    port map (ilf => t, znowptj => dlf, ptjglun => sjochdx);
end agpnmfq;

library ieee;
use ieee.std_logic_1164.all;

entity wz is
  port (bothm : inout std_logic; tpzxsx : inout real; b : buffer std_logic; jw : buffer time);
end wz;

library ieee;
use ieee.std_logic_1164.all;

architecture fdyxbtqvd of wz is
  signal tw : std_logic;
  signal mmcpvahrxu : time_vector(1 to 2);
begin
  njjyizim : entity work.qpd
    port map (ilf => mmcpvahrxu, znowptj => b, ptjglun => tw);
end fdyxbtqvd;



entity fj is
  port (oxkhyxmorx : in time_vector(2 to 3); purrscxej : buffer integer_vector(1 to 1));
end fj;

library ieee;
use ieee.std_logic_1164.all;

architecture frq of fj is
  signal s : std_logic;
  signal gz : time_vector(1 to 2);
  signal qmd : time;
  signal f : real;
  signal r : std_logic;
begin
  mozqnokqd : entity work.wz
    port map (bothm => r, tpzxsx => f, b => r, jw => qmd);
  wmwovuth : entity work.qpd
    port map (ilf => gz, znowptj => r, ptjglun => s);
end frq;



-- Seed after: 5626356544301872127,7332793847894666635
