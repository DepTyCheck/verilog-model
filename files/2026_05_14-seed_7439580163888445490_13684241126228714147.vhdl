-- Seed: 7439580163888445490,13684241126228714147

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (kjfwg : linkage integer; rgfj : in std_logic; wmag : linkage time; llaauwfn : out std_logic);
end e;



architecture zhkvvrz of e is
  
begin
  
end zhkvvrz;



entity txelfrae is
  port (ssnf : inout time);
end txelfrae;

library ieee;
use ieee.std_logic_1164.all;

architecture vkfu of txelfrae is
  signal lrloyctix : time;
  signal hzllxfifx : std_logic;
  signal bfh : integer;
  signal xsrjgf : time;
  signal ivywn : time;
  signal vhtpljbff : std_logic;
  signal pqjcwrxwzc : integer;
begin
  s : entity work.e
    port map (kjfwg => pqjcwrxwzc, rgfj => vhtpljbff, wmag => ivywn, llaauwfn => vhtpljbff);
  tnhi : entity work.e
    port map (kjfwg => pqjcwrxwzc, rgfj => vhtpljbff, wmag => xsrjgf, llaauwfn => vhtpljbff);
  ywi : entity work.e
    port map (kjfwg => bfh, rgfj => vhtpljbff, wmag => ssnf, llaauwfn => vhtpljbff);
  vhlf : entity work.e
    port map (kjfwg => pqjcwrxwzc, rgfj => hzllxfifx, wmag => lrloyctix, llaauwfn => vhtpljbff);
end vkfu;



entity bmmax is
  port (gesm : buffer severity_level; qyxhfxhmb : linkage integer);
end bmmax;

library ieee;
use ieee.std_logic_1164.all;

architecture hw of bmmax is
  signal zfxk : std_logic;
  signal sltvhkkm : std_logic;
  signal rwcgxi : integer;
  signal iiwg : time;
  signal otryb : std_logic;
  signal owmbmejygw : time;
begin
  clvqbc : entity work.txelfrae
    port map (ssnf => owmbmejygw);
  ad : entity work.e
    port map (kjfwg => qyxhfxhmb, rgfj => otryb, wmag => iiwg, llaauwfn => otryb);
  wqtb : entity work.e
    port map (kjfwg => rwcgxi, rgfj => sltvhkkm, wmag => iiwg, llaauwfn => zfxk);
end hw;



-- Seed after: 9515548625001041545,13684241126228714147
