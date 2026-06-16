-- Seed: 3948003932690978137,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (lqloncygwy : linkage time; qlzs : buffer std_logic; wulnnujmv : inout std_logic_vector(4 downto 0); uuzku : linkage integer);
end m;

architecture ylc of m is
  
begin
  -- Multi-driven assignments
  wulnnujmv <= ('L', 'Z', '0', 'W', '-');
  wulnnujmv <= "LWH0U";
  wulnnujmv <= "LWHHZ";
  wulnnujmv <= ('1', '0', '-', 'W', 'U');
end ylc;

entity po is
  port (ajwwyyhps : buffer real; hjtjwcfik : inout integer);
end po;

library ieee;
use ieee.std_logic_1164.all;

architecture w of po is
  signal fbnhwom : integer;
  signal afvc : std_logic_vector(4 downto 0);
  signal fmyjxcdj : std_logic;
  signal ngcltb : time;
begin
  p : entity work.m
    port map (lqloncygwy => ngcltb, qlzs => fmyjxcdj, wulnnujmv => afvc, uuzku => fbnhwom);
  
  -- Single-driven assignments
  ajwwyyhps <= 003.420;
  
  -- Multi-driven assignments
  fmyjxcdj <= 'U';
  fmyjxcdj <= 'L';
  fmyjxcdj <= 'Z';
end w;

library ieee;
use ieee.std_logic_1164.all;

entity gpgfua is
  port (edmix : inout std_logic_vector(1 to 1); j : inout real_vector(1 downto 3); gasqqng : out std_logic);
end gpgfua;

library ieee;
use ieee.std_logic_1164.all;

architecture quiqfesla of gpgfua is
  signal vqmw : integer;
  signal orixdrc : real;
  signal ukur : integer;
  signal y : std_logic;
  signal ihthaielka : time;
  signal jsbx : integer;
  signal lwlzfr : std_logic_vector(4 downto 0);
  signal pblmf : std_logic;
  signal tgrhhx : time;
begin
  yfygy : entity work.m
    port map (lqloncygwy => tgrhhx, qlzs => pblmf, wulnnujmv => lwlzfr, uuzku => jsbx);
  dlplatt : entity work.m
    port map (lqloncygwy => ihthaielka, qlzs => y, wulnnujmv => lwlzfr, uuzku => ukur);
  bdwd : entity work.po
    port map (ajwwyyhps => orixdrc, hjtjwcfik => vqmw);
  
  -- Single-driven assignments
  j <= (others => 0.0);
  
  -- Multi-driven assignments
  gasqqng <= 'X';
  gasqqng <= 'L';
  y <= '-';
  lwlzfr <= ('Z', 'Z', 'X', '1', 'Z');
end quiqfesla;



-- Seed after: 5112118086052274120,5472058987609252853
