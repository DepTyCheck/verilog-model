-- Seed: 14998332877385540411,16265041255589496407

library ieee;
use ieee.std_logic_1164.all;

entity wxczn is
  port (ktxznjts : in std_logic; tytu : inout bit_vector(4 to 2); aw : out std_logic; dy : inout std_logic);
end wxczn;



architecture hcidmzey of wxczn is
  
begin
  
end hcidmzey;



entity lpvrud is
  port (vhiw : buffer integer; zmuy : buffer time; krbsbhg : inout bit; sgpwnh : out real);
end lpvrud;

library ieee;
use ieee.std_logic_1164.all;

architecture jqr of lpvrud is
  signal z : std_logic;
  signal kjlocvyrg : bit_vector(4 to 2);
  signal oosyd : bit_vector(4 to 2);
  signal labq : std_logic;
begin
  gsebo : entity work.wxczn
    port map (ktxznjts => labq, tytu => oosyd, aw => labq, dy => labq);
  okcfqryj : entity work.wxczn
    port map (ktxznjts => labq, tytu => kjlocvyrg, aw => z, dy => z);
end jqr;

library ieee;
use ieee.std_logic_1164.all;

entity tjd is
  port (ksjoj : in std_logic_vector(2 downto 0));
end tjd;

library ieee;
use ieee.std_logic_1164.all;

architecture uwtm of tjd is
  signal uee : bit_vector(4 to 2);
  signal q : std_logic;
  signal m : real;
  signal rxl : bit;
  signal tiztazmk : time;
  signal k : integer;
begin
  kemvlt : entity work.lpvrud
    port map (vhiw => k, zmuy => tiztazmk, krbsbhg => rxl, sgpwnh => m);
  p : entity work.wxczn
    port map (ktxznjts => q, tytu => uee, aw => q, dy => q);
end uwtm;



-- Seed after: 4839030398619567285,16265041255589496407
