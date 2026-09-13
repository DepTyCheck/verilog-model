-- Seed: 1751676762613049556,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (jqlld : buffer real; krssiehu : out std_logic; s : in std_logic_vector(1 downto 0); phsjcrp : in real_vector(1 downto 3));
end n;

architecture ff of n is
  
begin
  -- Single-driven assignments
  jqlld <= 4.2;
  
  -- Multi-driven assignments
  krssiehu <= krssiehu;
  krssiehu <= 'H';
  krssiehu <= krssiehu;
  krssiehu <= krssiehu;
end ff;

entity elrbcxpwm is
  port (kyr : buffer time);
end elrbcxpwm;

library ieee;
use ieee.std_logic_1164.all;

architecture bjxiol of elrbcxpwm is
  signal jzonyaxo : std_logic;
  signal gtc : real;
  signal iu : real_vector(1 downto 3);
  signal a : std_logic_vector(1 downto 0);
  signal ewnnpai : std_logic;
  signal pd : real;
begin
  xjx : entity work.n
    port map (jqlld => pd, krssiehu => ewnnpai, s => a, phsjcrp => iu);
  l : entity work.n
    port map (jqlld => gtc, krssiehu => jzonyaxo, s => a, phsjcrp => iu);
  
  -- Single-driven assignments
  kyr <= 1 hr;
end bjxiol;

entity xjhkwmwos is
  port (usngfkq : in boolean; m : out real; nzyptptgvt : in time);
end xjhkwmwos;

library ieee;
use ieee.std_logic_1164.all;

architecture iwtqq of xjhkwmwos is
  signal crcfeusn : time;
  signal dek : real_vector(1 downto 3);
  signal wv : std_logic_vector(1 downto 0);
  signal a : real;
  signal clf : real_vector(1 downto 3);
  signal ymtwqkzsj : std_logic_vector(1 downto 0);
  signal hnu : std_logic;
  signal jr : real;
begin
  mhnsbhs : entity work.n
    port map (jqlld => jr, krssiehu => hnu, s => ymtwqkzsj, phsjcrp => clf);
  yubrlhtdcl : entity work.n
    port map (jqlld => a, krssiehu => hnu, s => wv, phsjcrp => clf);
  bxvtc : entity work.n
    port map (jqlld => m, krssiehu => hnu, s => wv, phsjcrp => dek);
  tvymvif : entity work.elrbcxpwm
    port map (kyr => crcfeusn);
end iwtqq;



-- Seed after: 4181400077246631238,10754487200446211253
