-- Seed: 17664388105501362078,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity dz is
  port (gtdbyc : in std_logic_vector(3 to 2); xdgrqyh : linkage std_logic; hgvpaqmtfb : linkage time);
end dz;

architecture wfbsag of dz is
  
begin
  
end wfbsag;

library ieee;
use ieee.std_logic_1164.all;

entity uhcetftax is
  port (gpiympmyi : in time; heqtfls : inout time; r : inout std_logic_vector(0 to 0));
end uhcetftax;

library ieee;
use ieee.std_logic_1164.all;

architecture kmotrbky of uhcetftax is
  signal gzgg : time;
  signal vopklslq : time;
  signal zjb : time;
  signal bcrbkc : std_logic;
  signal siyg : std_logic_vector(3 to 2);
begin
  nnglu : entity work.dz
    port map (gtdbyc => siyg, xdgrqyh => bcrbkc, hgvpaqmtfb => zjb);
  pmzjll : entity work.dz
    port map (gtdbyc => siyg, xdgrqyh => bcrbkc, hgvpaqmtfb => vopklslq);
  axuh : entity work.dz
    port map (gtdbyc => siyg, xdgrqyh => bcrbkc, hgvpaqmtfb => heqtfls);
  ij : entity work.dz
    port map (gtdbyc => siyg, xdgrqyh => bcrbkc, hgvpaqmtfb => gzgg);
end kmotrbky;

entity fj is
  port (rzn : out time; lpttob : buffer time);
end fj;

library ieee;
use ieee.std_logic_1164.all;

architecture aiczxnyxjs of fj is
  signal yyywuanw : std_logic;
  signal uqp : std_logic_vector(3 to 2);
  signal oes : std_logic_vector(0 to 0);
begin
  azftcpdsv : entity work.uhcetftax
    port map (gpiympmyi => rzn, heqtfls => lpttob, r => oes);
  vur : entity work.dz
    port map (gtdbyc => uqp, xdgrqyh => yyywuanw, hgvpaqmtfb => rzn);
  
  -- Multi-driven assignments
  uqp <= uqp;
  oes <= (others => 'L');
end aiczxnyxjs;

entity htccdjdwby is
  port (ekbhcga : out time);
end htccdjdwby;

library ieee;
use ieee.std_logic_1164.all;

architecture tnycmdui of htccdjdwby is
  signal uznkqfrijo : std_logic_vector(0 to 0);
  signal qununnkg : time;
  signal abjjcby : std_logic_vector(0 to 0);
begin
  onct : entity work.uhcetftax
    port map (gpiympmyi => ekbhcga, heqtfls => ekbhcga, r => abjjcby);
  j : entity work.uhcetftax
    port map (gpiympmyi => qununnkg, heqtfls => qununnkg, r => uznkqfrijo);
  
  -- Multi-driven assignments
  abjjcby <= uznkqfrijo;
  abjjcby <= "W";
end tnycmdui;



-- Seed after: 12227878892465485186,12536735141187299017
