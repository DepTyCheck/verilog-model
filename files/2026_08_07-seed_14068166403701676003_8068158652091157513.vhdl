-- Seed: 14068166403701676003,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity ujdjcgs is
  port (wjllnyphen : in std_logic_vector(3 to 2));
end ujdjcgs;

architecture ckhaxwg of ujdjcgs is
  
begin
  
end ckhaxwg;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (zhasn : buffer std_logic_vector(0 downto 4); ktnlkgdz : buffer time; qfw : linkage real);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture wzhqyzdi of o is
  signal ys : std_logic_vector(3 to 2);
begin
  ingnla : entity work.ujdjcgs
    port map (wjllnyphen => ys);
  ucvhan : entity work.ujdjcgs
    port map (wjllnyphen => zhasn);
  
  -- Multi-driven assignments
  zhasn <= (others => '0');
end wzhqyzdi;

library ieee;
use ieee.std_logic_1164.all;

entity lkoszue is
  port (xgb : out real; h : inout std_logic; anfkcllt : out std_logic_vector(1 downto 1));
end lkoszue;

library ieee;
use ieee.std_logic_1164.all;

architecture iczceisui of lkoszue is
  signal qcmobrqv : time;
  signal nfo : std_logic_vector(3 to 2);
begin
  l : entity work.o
    port map (zhasn => nfo, ktnlkgdz => qcmobrqv, qfw => xgb);
  gvkpb : entity work.ujdjcgs
    port map (wjllnyphen => nfo);
  vkfolartvi : entity work.ujdjcgs
    port map (wjllnyphen => nfo);
  lnp : entity work.ujdjcgs
    port map (wjllnyphen => nfo);
  
  -- Multi-driven assignments
  h <= 'W';
end iczceisui;



-- Seed after: 16636341608296035511,8068158652091157513
