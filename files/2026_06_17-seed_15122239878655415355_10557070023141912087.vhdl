-- Seed: 15122239878655415355,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity cvarnji is
  port (iipzkd : in time; tnjehqlf : buffer std_logic; zxd : linkage std_logic_vector(0 downto 3));
end cvarnji;

architecture svpl of cvarnji is
  
begin
  -- Multi-driven assignments
  tnjehqlf <= 'L';
end svpl;

entity fgwkui is
  port (cxffd : in boolean_vector(1 to 4); jolvoolahz : out bit_vector(4 downto 2); vaqapyrf : buffer time);
end fgwkui;

architecture gbetxa of fgwkui is
  
begin
  -- Single-driven assignments
  vaqapyrf <= 34 ms;
end gbetxa;

entity tssyri is
  port (q : inout time_vector(3 to 1); wepqptxulb : out bit_vector(2 downto 0); xhlaynubk : out time; htlk : buffer integer);
end tssyri;

architecture attbumsecn of tssyri is
  signal abp : boolean_vector(1 to 4);
begin
  ieot : entity work.fgwkui
    port map (cxffd => abp, jolvoolahz => wepqptxulb, vaqapyrf => xhlaynubk);
  
  -- Single-driven assignments
  htlk <= 8#0#;
  abp <= (FALSE, TRUE, TRUE, TRUE);
  q <= (others => 0 ns);
end attbumsecn;

entity pwiwrccm is
  port (hcy : in real; ipgr : in string(5 downto 5));
end pwiwrccm;

library ieee;
use ieee.std_logic_1164.all;

architecture quxzydj of pwiwrccm is
  signal ztqaqvma : std_logic_vector(0 downto 3);
  signal zwdnm : time;
  signal po : std_logic;
  signal mvum : std_logic_vector(0 downto 3);
  signal fpz : std_logic;
  signal h : time;
begin
  hbgn : entity work.cvarnji
    port map (iipzkd => h, tnjehqlf => fpz, zxd => mvum);
  papuwkvi : entity work.cvarnji
    port map (iipzkd => h, tnjehqlf => po, zxd => mvum);
  xkbcpby : entity work.cvarnji
    port map (iipzkd => zwdnm, tnjehqlf => fpz, zxd => ztqaqvma);
  
  -- Multi-driven assignments
  fpz <= 'L';
  fpz <= 'Z';
end quxzydj;



-- Seed after: 2850299148063474397,10557070023141912087
