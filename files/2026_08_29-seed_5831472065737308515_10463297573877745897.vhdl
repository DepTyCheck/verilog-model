-- Seed: 5831472065737308515,10463297573877745897

entity cqafexfou is
  port (udll : inout time);
end cqafexfou;

architecture uxcoo of cqafexfou is
  
begin
  -- Single-driven assignments
  udll <= udll;
end uxcoo;

entity zscnbspybw is
  port (dseeiuz : linkage boolean_vector(2 downto 2); uxsjsgn : buffer boolean_vector(1 downto 3); lcoshzm : buffer real);
end zscnbspybw;

architecture nsahmivsau of zscnbspybw is
  signal aetguepuv : time;
  signal lmraknvqvc : time;
  signal iztxmwlcf : time;
  signal wa : time;
begin
  riibqy : entity work.cqafexfou
    port map (udll => wa);
  yf : entity work.cqafexfou
    port map (udll => iztxmwlcf);
  llxxwzssfh : entity work.cqafexfou
    port map (udll => lmraknvqvc);
  jurcy : entity work.cqafexfou
    port map (udll => aetguepuv);
end nsahmivsau;

library ieee;
use ieee.std_logic_1164.all;

entity cqsfvxtvb is
  port (ztqwpvz : out std_logic_vector(4 to 4); obuwyfw : linkage integer; gqxia : linkage time);
end cqsfvxtvb;

architecture ths of cqsfvxtvb is
  signal gtqfukf : real;
  signal qkpudgdcm : boolean_vector(1 downto 3);
  signal ppqzcawbo : boolean_vector(2 downto 2);
begin
  ye : entity work.zscnbspybw
    port map (dseeiuz => ppqzcawbo, uxsjsgn => qkpudgdcm, lcoshzm => gtqfukf);
  
  -- Multi-driven assignments
  ztqwpvz <= (others => '-');
  ztqwpvz <= ztqwpvz;
  ztqwpvz <= ztqwpvz;
  ztqwpvz <= (others => '-');
end ths;

entity vjybhilij is
  port (fyd : in integer);
end vjybhilij;

library ieee;
use ieee.std_logic_1164.all;

architecture vapio of vjybhilij is
  signal kiv : time;
  signal sghuawij : time;
  signal y : integer;
  signal chfbgkut : std_logic_vector(4 to 4);
  signal lsykknsffp : time;
  signal arhv : time;
begin
  pqtgzmjvu : entity work.cqafexfou
    port map (udll => arhv);
  itrzwlb : entity work.cqafexfou
    port map (udll => lsykknsffp);
  yupxlbjodd : entity work.cqsfvxtvb
    port map (ztqwpvz => chfbgkut, obuwyfw => y, gqxia => sghuawij);
  lk : entity work.cqafexfou
    port map (udll => kiv);
  
  -- Multi-driven assignments
  chfbgkut <= chfbgkut;
  chfbgkut <= (others => '1');
end vapio;



-- Seed after: 2294921256439939029,10463297573877745897
