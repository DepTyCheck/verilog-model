-- Seed: 1534255796389867412,1852660963590380551



entity ahwnmok is
  port (t : inout time; mhq : in time);
end ahwnmok;



architecture g of ahwnmok is
  
begin
  
end g;

library ieee;
use ieee.std_logic_1164.all;

entity fuanlw is
  port (hoda : linkage std_logic; lg : inout integer);
end fuanlw;



architecture cocliwrrm of fuanlw is
  signal zfymnd : time;
  signal twgqosoa : time;
  signal nr : time;
  signal ghes : time;
begin
  rtdtlpigm : entity work.ahwnmok
    port map (t => ghes, mhq => nr);
  enhjgz : entity work.ahwnmok
    port map (t => twgqosoa, mhq => zfymnd);
  qxvftitnia : entity work.ahwnmok
    port map (t => zfymnd, mhq => ghes);
end cocliwrrm;



entity kiof is
  port (gbzgwtrbp : in real; hyzewqz : out bit_vector(2 to 3));
end kiof;

library ieee;
use ieee.std_logic_1164.all;

architecture jbypjox of kiof is
  signal dautswj : integer;
  signal se : std_logic;
begin
  zl : entity work.fuanlw
    port map (hoda => se, lg => dautswj);
end jbypjox;

library ieee;
use ieee.std_logic_1164.all;

entity gd is
  port (tbzbmwyp : in integer; w : in std_logic);
end gd;

library ieee;
use ieee.std_logic_1164.all;

architecture yifx of gd is
  signal tparp : integer;
  signal bnaxgrpz : std_logic;
  signal g : time;
  signal eg : time;
  signal meqsbgac : integer;
begin
  fpvjobcj : entity work.fuanlw
    port map (hoda => w, lg => meqsbgac);
  vhigmcfma : entity work.ahwnmok
    port map (t => eg, mhq => g);
  vxgiqiycp : entity work.fuanlw
    port map (hoda => bnaxgrpz, lg => tparp);
end yifx;



-- Seed after: 14157416209408178460,1852660963590380551
