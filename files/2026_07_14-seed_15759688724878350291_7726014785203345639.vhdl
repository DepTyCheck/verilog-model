-- Seed: 15759688724878350291,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity pzc is
  port (qyhwpolsn : linkage std_logic_vector(4 downto 0));
end pzc;

architecture h of pzc is
  
begin
  
end h;

use std.reflection.all;

entity donkaf is
  port (duuloteku : inout integer_value_mirror);
end donkaf;

library ieee;
use ieee.std_logic_1164.all;

architecture gnzv of donkaf is
  signal xpvcy : std_logic_vector(4 downto 0);
  signal duqttygtoz : std_logic_vector(4 downto 0);
begin
  saswjhtuoq : entity work.pzc
    port map (qyhwpolsn => duqttygtoz);
  elzeiygngk : entity work.pzc
    port map (qyhwpolsn => duqttygtoz);
  nofsqkmi : entity work.pzc
    port map (qyhwpolsn => xpvcy);
  cqx : entity work.pzc
    port map (qyhwpolsn => xpvcy);
  
  -- Multi-driven assignments
  duqttygtoz <= "XX1Z0";
  duqttygtoz <= "WXWLL";
end gnzv;

entity uhvvjk is
  port (mpkuldpnj : out boolean_vector(2 to 1));
end uhvvjk;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture hgevc of uhvvjk is
  signal j : std_logic_vector(4 downto 0);
  signal wvvbh : std_logic_vector(4 downto 0);
  shared variable ddbhaif : integer_value_mirror;
  signal egwphxfzb : std_logic_vector(4 downto 0);
begin
  rgfxrdmdws : entity work.pzc
    port map (qyhwpolsn => egwphxfzb);
  yoxcwwwj : entity work.donkaf
    port map (duuloteku => ddbhaif);
  zbnnyke : entity work.pzc
    port map (qyhwpolsn => wvvbh);
  gmxieg : entity work.pzc
    port map (qyhwpolsn => j);
  
  -- Single-driven assignments
  mpkuldpnj <= mpkuldpnj;
end hgevc;



-- Seed after: 7169769717308567474,7726014785203345639
