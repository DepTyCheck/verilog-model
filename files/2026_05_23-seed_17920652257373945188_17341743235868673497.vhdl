-- Seed: 17920652257373945188,17341743235868673497

library ieee;
use ieee.std_logic_1164.all;

entity fpt is
  port (mcxqnv : out time; ua : in std_logic; xmy : out real);
end fpt;



architecture eysdbr of fpt is
  
begin
  
end eysdbr;



entity hvxbr is
  port (sxpwplwk : linkage real; vugi : buffer real; mghotscfj : inout real);
end hvxbr;

library ieee;
use ieee.std_logic_1164.all;

architecture kiubfxfgsy of hvxbr is
  signal obzqtqibb : std_logic;
  signal arxrtucl : time;
begin
  y : entity work.fpt
    port map (mcxqnv => arxrtucl, ua => obzqtqibb, xmy => mghotscfj);
end kiubfxfgsy;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (vkx : buffer std_logic);
end y;



architecture dznxm of y is
  signal dpo : real;
  signal vu : real;
  signal audnnkx : real;
begin
  jtf : entity work.hvxbr
    port map (sxpwplwk => audnnkx, vugi => vu, mghotscfj => dpo);
end dznxm;



entity b is
  port (igfatp : inout integer; piexa : in time);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture qokkdfvw of b is
  signal vwv : real;
  signal ijizfhls : std_logic;
  signal kbfz : time;
  signal evjh : std_logic;
  signal yigdtgr : real;
  signal qf : real;
  signal l : real;
begin
  aiftxbqw : entity work.hvxbr
    port map (sxpwplwk => l, vugi => qf, mghotscfj => yigdtgr);
  lftrq : entity work.y
    port map (vkx => evjh);
  upaxn : entity work.fpt
    port map (mcxqnv => kbfz, ua => ijizfhls, xmy => vwv);
end qokkdfvw;



-- Seed after: 10069578785595347902,17341743235868673497
