-- Seed: 3151229154713588071,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity nh is
  port (biqj : buffer real; wuinawred : inout std_logic; ludfjxgxgy : buffer bit_vector(1 downto 0); pphhqkgsow : inout time);
end nh;

architecture czw of nh is
  
begin
  -- Single-driven assignments
  pphhqkgsow <= pphhqkgsow;
  biqj <= biqj;
  ludfjxgxgy <= ('1', '1');
  
  -- Multi-driven assignments
  wuinawred <= wuinawred;
  wuinawred <= wuinawred;
  wuinawred <= wuinawred;
end czw;

library ieee;
use ieee.std_logic_1164.all;

entity acmw is
  port (ygzzq : buffer std_logic; bjr : in std_logic_vector(2 downto 1); sh : buffer std_logic);
end acmw;

architecture bkzzibi of acmw is
  
begin
  
end bkzzibi;

entity cwjwexh is
  port (t : linkage real);
end cwjwexh;

library ieee;
use ieee.std_logic_1164.all;

architecture zprzoqk of cwjwexh is
  signal vrymmzcm : time;
  signal qlpw : bit_vector(1 downto 0);
  signal xgv : std_logic;
  signal mm : real;
  signal ngxsxfjqxt : time;
  signal bcdxhd : bit_vector(1 downto 0);
  signal yh : std_logic;
  signal qeqveljzka : real;
begin
  qykowxc : entity work.nh
    port map (biqj => qeqveljzka, wuinawred => yh, ludfjxgxgy => bcdxhd, pphhqkgsow => ngxsxfjqxt);
  okpv : entity work.nh
    port map (biqj => mm, wuinawred => xgv, ludfjxgxgy => qlpw, pphhqkgsow => vrymmzcm);
end zprzoqk;

library ieee;
use ieee.std_logic_1164.all;

entity hptvvwhz is
  port (iqbtya : out std_logic);
end hptvvwhz;

library ieee;
use ieee.std_logic_1164.all;

architecture bhwexb of hptvvwhz is
  signal afebmmd : time;
  signal qqv : bit_vector(1 downto 0);
  signal dmnyqaw : std_logic;
  signal cajevuhdw : real;
  signal psxksyo : time;
  signal nxxvtqri : bit_vector(1 downto 0);
  signal lncqxxc : real;
  signal kutbkry : std_logic;
  signal ss : std_logic_vector(2 downto 1);
  signal b : std_logic_vector(2 downto 1);
begin
  kj : entity work.acmw
    port map (ygzzq => iqbtya, bjr => b, sh => iqbtya);
  ixmvn : entity work.acmw
    port map (ygzzq => iqbtya, bjr => ss, sh => kutbkry);
  iy : entity work.nh
    port map (biqj => lncqxxc, wuinawred => iqbtya, ludfjxgxgy => nxxvtqri, pphhqkgsow => psxksyo);
  byad : entity work.nh
    port map (biqj => cajevuhdw, wuinawred => dmnyqaw, ludfjxgxgy => qqv, pphhqkgsow => afebmmd);
  
  -- Multi-driven assignments
  kutbkry <= 'X';
  kutbkry <= iqbtya;
  ss <= ss;
end bhwexb;



-- Seed after: 11258908816463543462,6000118208082478503
