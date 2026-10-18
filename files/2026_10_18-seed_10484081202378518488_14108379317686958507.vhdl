-- Seed: 10484081202378518488,14108379317686958507

entity fpoodcxs is
  port (zrxzb : out time_vector(0 downto 3); pdxscfbc : inout integer_vector(4 downto 4); ztwmyl : in bit);
end fpoodcxs;

architecture ium of fpoodcxs is
  
begin
  -- Single-driven assignments
  zrxzb <= (others => 0 ns);
  pdxscfbc <= (others => 16#1#);
end ium;

entity rjnvyhblx is
  port (xxrfsyyi : linkage real);
end rjnvyhblx;

architecture lgdrluvpzo of rjnvyhblx is
  signal w : integer_vector(4 downto 4);
  signal pvnsfyiutm : time_vector(0 downto 3);
  signal uwo : bit;
  signal tdbwy : integer_vector(4 downto 4);
  signal fmnkbzihel : time_vector(0 downto 3);
begin
  arxkgq : entity work.fpoodcxs
    port map (zrxzb => fmnkbzihel, pdxscfbc => tdbwy, ztwmyl => uwo);
  gbb : entity work.fpoodcxs
    port map (zrxzb => pvnsfyiutm, pdxscfbc => w, ztwmyl => uwo);
  
  -- Single-driven assignments
  uwo <= '1';
end lgdrluvpzo;

entity kekgdxfdp is
  port (jyx : in integer_vector(0 downto 1); eyauyz : out time);
end kekgdxfdp;

architecture j of kekgdxfdp is
  signal imcfbrixhx : bit;
  signal onpy : integer_vector(4 downto 4);
  signal xtz : time_vector(0 downto 3);
  signal jafds : bit;
  signal mbkskh : integer_vector(4 downto 4);
  signal q : time_vector(0 downto 3);
  signal gup : bit;
  signal pxnlausom : integer_vector(4 downto 4);
  signal ri : time_vector(0 downto 3);
begin
  x : entity work.fpoodcxs
    port map (zrxzb => ri, pdxscfbc => pxnlausom, ztwmyl => gup);
  zautseco : entity work.fpoodcxs
    port map (zrxzb => q, pdxscfbc => mbkskh, ztwmyl => jafds);
  utg : entity work.fpoodcxs
    port map (zrxzb => xtz, pdxscfbc => onpy, ztwmyl => imcfbrixhx);
end j;

library ieee;
use ieee.std_logic_1164.all;

entity vzknbo is
  port (mfsewrgt : in integer; mnpwpxlox : buffer std_logic; lwy : inout real_vector(3 downto 2); u : out std_logic);
end vzknbo;

architecture kzzmp of vzknbo is
  signal exg : bit;
  signal spfkckab : integer_vector(4 downto 4);
  signal fxl : time_vector(0 downto 3);
  signal yolmy : real;
  signal lgbkf : bit;
  signal tkbbolfmq : integer_vector(4 downto 4);
  signal krqfwjrsgp : time_vector(0 downto 3);
  signal abqsertuz : real;
begin
  wgma : entity work.rjnvyhblx
    port map (xxrfsyyi => abqsertuz);
  pghadp : entity work.fpoodcxs
    port map (zrxzb => krqfwjrsgp, pdxscfbc => tkbbolfmq, ztwmyl => lgbkf);
  horhbzts : entity work.rjnvyhblx
    port map (xxrfsyyi => yolmy);
  dtol : entity work.fpoodcxs
    port map (zrxzb => fxl, pdxscfbc => spfkckab, ztwmyl => exg);
  
  -- Single-driven assignments
  exg <= lgbkf;
  lgbkf <= exg;
  
  -- Multi-driven assignments
  mnpwpxlox <= 'L';
  u <= 'U';
  u <= mnpwpxlox;
end kzzmp;



-- Seed after: 17484395454344591858,14108379317686958507
