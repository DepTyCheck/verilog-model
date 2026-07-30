-- Seed: 7725370428423375076,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity mcbjcpszh is
  port (wwxbzauc : inout std_logic_vector(3 to 0); u : out time);
end mcbjcpszh;

architecture m of mcbjcpszh is
  
begin
  -- Single-driven assignments
  u <= u;
  
  -- Multi-driven assignments
  wwxbzauc <= "";
  wwxbzauc <= wwxbzauc;
end m;

entity xw is
  port (cqmllga : in time; xfubgjhcah : buffer integer);
end xw;

library ieee;
use ieee.std_logic_1164.all;

architecture kvzyegwhvt of xw is
  signal hvblv : time;
  signal tcexcy : time;
  signal pbzmj : time;
  signal wah : std_logic_vector(3 to 0);
begin
  smheb : entity work.mcbjcpszh
    port map (wwxbzauc => wah, u => pbzmj);
  nthiagyax : entity work.mcbjcpszh
    port map (wwxbzauc => wah, u => tcexcy);
  eoczvmfyjh : entity work.mcbjcpszh
    port map (wwxbzauc => wah, u => hvblv);
  
  -- Single-driven assignments
  xfubgjhcah <= 0_4;
end kvzyegwhvt;

library ieee;
use ieee.std_logic_1164.all;

entity ibzltbzx is
  port (fp : buffer time; zmfnuyyhi : inout real; a : inout std_logic);
end ibzltbzx;

library ieee;
use ieee.std_logic_1164.all;

architecture rnciadiap of ibzltbzx is
  signal tfkl : time;
  signal dkbmauh : std_logic_vector(3 to 0);
begin
  pgdnf : entity work.mcbjcpszh
    port map (wwxbzauc => dkbmauh, u => tfkl);
  
  -- Single-driven assignments
  zmfnuyyhi <= 414.3_2_2_0_4;
  fp <= fp;
  
  -- Multi-driven assignments
  a <= 'X';
  a <= a;
  a <= '0';
end rnciadiap;



-- Seed after: 8902936292370740948,4122021602305298647
