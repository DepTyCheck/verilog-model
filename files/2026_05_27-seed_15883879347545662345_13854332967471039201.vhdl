-- Seed: 15883879347545662345,13854332967471039201

library ieee;
use ieee.std_logic_1164.all;

entity bdvhmtenq is
  port (w : buffer time; rkabq : linkage std_logic_vector(3 downto 0); hbjhva : buffer boolean);
end bdvhmtenq;



architecture uvyqxrk of bdvhmtenq is
  
begin
  
end uvyqxrk;

library ieee;
use ieee.std_logic_1164.all;

entity qzh is
  port (lprroldt : in std_logic_vector(0 to 3));
end qzh;

library ieee;
use ieee.std_logic_1164.all;

architecture akg of qzh is
  signal isv : boolean;
  signal kq : time;
  signal cdkf : boolean;
  signal iwmwcqw : time;
  signal s : boolean;
  signal h : std_logic_vector(3 downto 0);
  signal nee : time;
begin
  kknppve : entity work.bdvhmtenq
    port map (w => nee, rkabq => h, hbjhva => s);
  fji : entity work.bdvhmtenq
    port map (w => iwmwcqw, rkabq => lprroldt, hbjhva => cdkf);
  zudla : entity work.bdvhmtenq
    port map (w => kq, rkabq => lprroldt, hbjhva => isv);
end akg;

library ieee;
use ieee.std_logic_1164.all;

entity cl is
  port (gu : in std_logic; nzrtg : linkage integer);
end cl;

library ieee;
use ieee.std_logic_1164.all;

architecture cy of cl is
  signal vezsexixpg : boolean;
  signal yaezoao : time;
  signal csf : boolean;
  signal tcmgyit : time;
  signal nkgnjyp : boolean;
  signal ojd : std_logic_vector(3 downto 0);
  signal iakn : time;
begin
  b : entity work.bdvhmtenq
    port map (w => iakn, rkabq => ojd, hbjhva => nkgnjyp);
  nhabixhflx : entity work.qzh
    port map (lprroldt => ojd);
  gdvqqzs : entity work.bdvhmtenq
    port map (w => tcmgyit, rkabq => ojd, hbjhva => csf);
  inislxz : entity work.bdvhmtenq
    port map (w => yaezoao, rkabq => ojd, hbjhva => vezsexixpg);
end cy;



-- Seed after: 3331339749897758505,13854332967471039201
